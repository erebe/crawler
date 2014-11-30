{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module RestAPI (runServer) where

import           Service

import qualified Eztv                      as Serie
import qualified Haruhichan                as Anime
import qualified OpenWeather               as Weather
import qualified Reddit
import qualified Youtube

import           Control.Applicative       ((<$>))
import           Control.Concurrent.MVar   (MVar, readMVar)
import           Data.Maybe

import           Control.Lens              hiding ((.=))
import           Control.Monad.IO.Class    (liftIO)
import           Network.HTTP.Types.Status (ok200)
import           Web.Scotty

import qualified Data.Text                 as T

import           Data.Aeson                hiding (json)
import           Data.Aeson.TH

import           Data.List


import           Data.UnixTime

instance ToJSON UnixTime where
    toJSON = toJSON . show . utSeconds

#define JSON_OPTIONS defaultOptions {fieldLabelModifier = dropWhile (== '_')}
$(deriveToJSON JSON_OPTIONS ''Weather.Forecast)
$(deriveToJSON JSON_OPTIONS ''Weather.Weather)

$(deriveToJSON JSON_OPTIONS ''Serie.Episode)
$(deriveToJSON JSON_OPTIONS ''Serie.Serie)

$(deriveToJSON JSON_OPTIONS ''Youtube.Video)
$(deriveToJSON JSON_OPTIONS ''Youtube.Channel)

$(deriveToJSON JSON_OPTIONS ''Anime.Episode)
$(deriveToJSON JSON_OPTIONS ''Anime.Anime)

$(deriveToJSON JSON_OPTIONS ''Reddit.Topic)
$(deriveToJSON JSON_OPTIONS ''Reddit.Reddit)
#undef JSON_OPTIONS


class APIVerb (a :: ServiceKind) where
    listA :: Service a -> ActionM ()
    lastA :: Service a -> ActionM ()
    findA :: String -> Service a -> ActionM ()

instance APIVerb Service.Any where
    listA (YoutubeS dat)  = listA dat
    listA (SerieS dat)    = listA dat
    listA (AnimeS dat)    = listA dat
    listA (RedditS dat)   = listA dat
    listA (ForecastS dat) = listA dat

    lastA (YoutubeS dat)  = lastA dat
    lastA (SerieS dat)    = lastA dat
    lastA (AnimeS dat)    = lastA dat
    lastA (RedditS dat)   = lastA dat
    lastA (ForecastS dat) = lastA dat

    findA arg (YoutubeS dat)  = findA arg dat
    findA arg (SerieS dat)    = findA arg dat
    findA arg (AnimeS dat)    = findA arg dat
    findA arg (RedditS dat)   = findA arg dat
    findA arg (ForecastS dat) = findA arg dat


instance APIVerb Service.Youtube where
    listA (MkYoutube ctx)        = json $ outputs ctx^..traverse.Youtube.name
    lastA (MkYoutube ctx)        = json  [channel & Youtube.videos .~ take 1 (channel^.Youtube.videos) | channel <- outputs ctx]
    findA toFind (MkYoutube ctx) = json $ outputs ctx^..traversed.filtered (\channel -> toFind `isInfixOf` (channel^.Youtube.name))

instance APIVerb Service.Serie where
    listA (MkSerie ctx)        = json $ outputs ctx^..traverse.Serie.name
    lastA (MkSerie ctx)        = json [serie & Serie.episodes .~ take 1 (serie^.Serie.episodes) | serie <- outputs ctx]
    findA toFind (MkSerie ctx) = json $ outputs ctx^..traversed.filtered (\serie -> toFind `isInfixOf` (serie^.Serie.name))

instance APIVerb Service.Anime where
    listA (MkAnime ctx)        = json $ outputs ctx^..traverse.Anime.name
    lastA (MkAnime ctx)        = json [ anime & Anime.episodes .~ take 1 (anime^.Anime.episodes) | anime <- outputs ctx]
    findA toFind (MkAnime ctx) = json $ outputs ctx^..traversed.filtered (\anime -> toFind `isInfixOf` (anime^.Anime.name))

instance APIVerb Service.Reddit where
    listA (MkReddit ctx)        = json $ outputs ctx^..traverse.Reddit.name
    lastA (MkReddit ctx)        = json [ reddit & Reddit.topics .~ take 25 (reddit^.Reddit.topics) | reddit <- outputs ctx ]
    findA toFind (MkReddit ctx) = json $ outputs ctx^..traversed.filtered (\reddit -> toFind `isInfixOf` (reddit^.Reddit.name))

instance APIVerb Service.Forecast where
    listA (MkForecast ctx)        = json $ Weather.city <$> outputs ctx
    lastA (MkForecast ctx)        = json [Weather.Weather (Weather.city city) (take 1 $ Weather.forecasts city) | city <- outputs ctx]
    findA toFind (MkForecast ctx) = json [ city | city <- outputs ctx, T.toLower (T.pack toFind)
                                                                                        `T.isInfixOf`
                                                                                          T.toLower (Weather.city city)]

dispatch :: APIVerb a => String -> Service a ->  ActionM ()
dispatch action service = case action of
                               "list" -> listA service
                               "last" -> lastA service
                               ""     -> lastA service
                               _      -> findA action service




runServer ::  MVar [Service Any] -> Int -> IO ()
runServer queue port = scotty port $ do
    get "/api/:type/:val" $ do
        service   <- param "type" :: ActionM String
        action    <- param "val"  :: ActionM String
        services  <- liftIO $ readMVar queue

        let requestResult = dispatch action <$> find ((service ==) . name) services

        fromMaybe next requestResult


    get "/assets/:folder/:file" $ do
        folderName <- param "folder" :: ActionM String
        fileName   <- param "file"   :: ActionM String
        file ("resources/" ++ folderName ++ "/" ++ fileName)


    notFound $ do
        status ok200
        setHeader "Content-type" "text/html; charset=utf-8"
        file "resources/index.html"



