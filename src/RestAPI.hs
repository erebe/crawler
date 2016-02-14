{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module RestAPI (runServer) where

import           ClassyPrelude
import           Service

import qualified Haruhichan                as Anime
import qualified OpenWeather               as Weather
import qualified Reddit                    as R
import qualified ShowRss                   as Serie
import qualified Youtube                   as Y

import           Control.Lens              hiding ((.=))

import           Data.Aeson                hiding (json)
import           Data.Aeson.TH

import qualified Data.Text                 as T
import           Data.UnixTime

import           Data.HList
import           Network.HTTP.Types.Status (ok200)
import           Web.Scotty

instance ToJSON UnixTime where
    toJSON = toJSON . show . utSeconds

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Weather.Forecast)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Weather.Weather)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Serie.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Serie.Serie)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Y.Video)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Y.Channel)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Anime.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Anime.Anime)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''R.Topic)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''R.Reddit)


class APIVerb (a :: ServiceKind) where
    listA :: Service a -> ActionM ()
    lastA :: Service a -> ActionM ()
    findA :: T.Text -> Service a -> ActionM ()

instance APIVerb 'Youtube where
  listA ctx = json $ outputs ctx^..traverse.Y.name
  lastA ctx = json  [channel & Y.videos .~ take 1 (channel^.Y.videos)
                    | channel <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                               (\channel -> toFind == T.toUpper (channel^.Y.name))

instance APIVerb 'Serie where
  listA ctx = json $ outputs ctx^..traverse.Serie.name
  lastA ctx = json [serie & Serie.episodes .~ take 1 (serie^.Serie.episodes)
                           | serie <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                             (\serie -> toFind == T.toUpper (serie^.Serie.name))

instance APIVerb 'Anime where
  listA ctx = json $ outputs ctx^..traverse.Anime.name
  lastA ctx = json [anime & Anime.episodes .~ take 1 (anime^.Anime.episodes)
                           | anime <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                             (\anime -> toFind == T.toUpper (anime^.Anime.name))

instance APIVerb 'Reddit where
  listA ctx = json $ outputs ctx^..traverse.R.name
  lastA ctx = json [reddit & R.topics .~ take 25 (reddit^.R.topics)
                            | reddit <- outputs ctx]
  findA toFind ctx = json $ outputs ctx^..traversed.filtered
                              (\reddit -> toFind == T.toUpper (reddit^.R.name))

-- instance APIVerb Forecast where
--   name _ = "forecast"
--   listA ctx = json $ Weather.city <$> outputs ctx
--   lastA ctx = json [Weather.Weather (Weather.city city) (take 1 $ Weather.forecasts city)
--                               | city <- outputs ctx]
--   findA toFind ctx = json [city | city <- outputs ctx
--                                 , toFind == T.toUpper (Weather.city city)]

dispatch :: APIVerb (a :: ServiceKind) => String -> String -> (Service a ->  ActionM ())
dispatch action target = case action of
                               "get" -> findA (T.toUpper . T.pack $ target)
                               "list" -> listA
                               "last" -> lastA
                               _      -> lastA

getService :: Text -> Services ['Youtube, 'Reddit, 'Serie, 'Anime] -> String -> String -> Maybe (ActionM ())
getService ((name (Proxy :: Proxy 'Youtube) ==) -> True) l action target = dispatch action target <$> (hOccursFst l :: Maybe (Service 'Youtube))
getService ((name (Proxy :: Proxy 'Reddit) ==) -> True) l action target = dispatch action target <$> (hOccursFst l :: Maybe (Service 'Reddit))
getService ((name (Proxy :: Proxy 'Serie) ==) -> True) l action target = dispatch action target <$> (hOccursFst l :: Maybe (Service 'Serie))
getService ((name (Proxy :: Proxy 'Anime) ==) -> True) l action target = dispatch action target <$> (hOccursFst l :: Maybe (Service 'Anime))
getService _ _ _ _ = Nothing



runServer ::  Int -> String -> MVar (Services ['Youtube, 'Reddit, 'Serie, 'Anime]) -> IO ()
runServer port homepagePath queue = scotty port $ do

    get (regex "^/api/([^/]+)/([^/]+)/?(.*)") $ do
      service   <- param "1" :: ActionM Text
      action    <- param "2" :: ActionM String
      target    <- param "3" :: ActionM String
      services  <- liftIO $ readMVar queue
      let requestResult = getService service services action target

      fromMaybe next requestResult


    get "/assets/:folder/:file" $ do
      folderName <- param "folder" :: ActionM String
      fileName   <- param "file"   :: ActionM String
      file (homepagePath <> "/" <> folderName <> "/" <> fileName)

    get "/favicon.ico"
      (file $ homepagePath <> "favicon.ico")


    notFound $ do
      status ok200
      setHeader "Content-type" "text/html; charset=utf-8"
      file (homepagePath <> "index.html")



