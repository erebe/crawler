{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RestAPI (runServer) where

import           Service

import qualified Haruhichan                as Anime
import qualified OpenWeather               as Weather
import qualified Reddit
import qualified ShowRss                   as Serie
import qualified Youtube

import           Control.Concurrent.MVar   (MVar, readMVar)
import           Control.Lens              hiding ((.=))
import           Control.Monad.IO.Class    (liftIO)

import           Data.Aeson                hiding (json)
import           Data.Aeson.TH

import           Data.List
import           Data.Maybe
import qualified Data.Text                 as T
import           Data.UnixTime
import           Network.HTTP.Types.Status (ok200)
import           Web.Scotty

instance ToJSON UnixTime where
    toJSON = toJSON . show . utSeconds

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Weather.Forecast)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Weather.Weather)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Serie.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Serie.Serie)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Youtube.Video)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Youtube.Channel)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Anime.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Anime.Anime)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Reddit.Topic)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Reddit.Reddit)


class APIVerb a where
    name  :: a -> String
    listA :: a -> ActionM ()
    lastA :: a -> ActionM ()
    findA :: T.Text -> a -> ActionM ()

instance APIVerb ServiceDTO where
  name (YoutubeDTO dat)  = name dat
  name (SerieDTO dat)    = name dat
  name (AnimeDTO dat)    = name dat
  name (RedditDTO dat)   = name dat
  name (ForecastDTO dat) = name dat

  listA (YoutubeDTO dat)  = listA dat
  listA (SerieDTO dat)    = listA dat
  listA (AnimeDTO dat)    = listA dat
  listA (RedditDTO dat)   = listA dat
  listA (ForecastDTO dat) = listA dat

  lastA (YoutubeDTO dat)  = lastA dat
  lastA (SerieDTO dat)    = lastA dat
  lastA (AnimeDTO dat)    = lastA dat
  lastA (RedditDTO dat)   = lastA dat
  lastA (ForecastDTO dat) = lastA dat

  findA arg (YoutubeDTO dat)  = findA arg dat
  findA arg (SerieDTO dat)    = findA arg dat
  findA arg (AnimeDTO dat)    = findA arg dat
  findA arg (RedditDTO dat)   = findA arg dat
  findA arg (ForecastDTO dat) = findA arg dat


instance APIVerb Youtube where
  name _ = "youtube"
  listA ctx = json $ outputs ctx^..traverse.Youtube.name
  lastA ctx = json  [channel & Youtube.videos .~ take 1 (channel^.Youtube.videos)
                    | channel <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                               (\channel -> toFind == T.toUpper (channel^.Youtube.name))

instance APIVerb Serie where
  name _ = "serie"
  listA ctx = json $ outputs ctx^..traverse.Serie.name
  lastA ctx = json [serie & Serie.episodes .~ take 1 (serie^.Serie.episodes)
                           | serie <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                             (\serie -> toFind == T.toUpper (serie^.Serie.name))

instance APIVerb Anime where
  name _ = "anime"
  listA ctx = json $ outputs ctx^..traverse.Anime.name
  lastA ctx = json [anime & Anime.episodes .~ take 1 (anime^.Anime.episodes)
                           | anime <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                             (\anime -> toFind == T.toUpper (anime^.Anime.name))

instance APIVerb Reddit where
  name _ = "reddit"
  listA ctx = json $ outputs ctx^..traverse.Reddit.name
  lastA ctx = json [reddit & Reddit.topics .~ take 25 (reddit^.Reddit.topics)
                            | reddit <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                              (\reddit -> toFind == T.toUpper (reddit^.Reddit.name))

instance APIVerb Forecast where
  name _ = "forecast"
  listA ctx = json $ Weather.city <$> outputs ctx
  lastA ctx = json [Weather.Weather (Weather.city city) (take 1 $ Weather.forecasts city)
                              | city <- outputs ctx]
  findA toFind ctx = json [city | city <- outputs ctx
                                , toFind == T.toUpper (Weather.city city)]

dispatch :: APIVerb a => String -> a ->  ActionM ()
dispatch action service = case action of
                               "list" -> listA service
                               "last" -> lastA service
                               ""     -> lastA service
                               _      -> findA (T.toUpper . T.pack $ action) service


runServer ::  MVar [ServiceDTO] -> Int -> IO ()
runServer queue port = scotty port $ do

    get (regex "^/api/([^/]+)/(.*)") $ do
      service   <- param "1" :: ActionM String
      action    <- param "2"  :: ActionM String
      services  <- liftIO $ readMVar queue
      let requestResult = dispatch action <$> find ((service ==) . name) services

      fromMaybe next requestResult


    get "/assets/:folder/:file" $ do
      folderName <- param "folder" :: ActionM String
      fileName   <- param "file"   :: ActionM String
      file ("thirdparty/homepage/" ++ folderName ++ "/" ++ fileName)

    get "/favicon.ico"
      (file "thirdparty/homepage/favicon.ico")


    notFound $ do
      status ok200
      setHeader "Content-type" "text/html; charset=utf-8"
      file "thirdparty/homepage/index.html"


