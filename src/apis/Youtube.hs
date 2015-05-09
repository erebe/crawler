{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Youtube ( Video()
               , videos, name
               , Channel()
               , title, url, thumbnail, date
               , fetch
               ) where

import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL


import           Data.Maybe

import           Control.Lens

import           Data.UnixTime

import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Control.Monad        (join)
import           Data.Aeson
import           Data.Aeson.Lens

data Video = Video { _title     :: T.Text
                   , _url       :: T.Text
                   , _thumbnail :: T.Text
                   , _date      :: T.Text
                   } deriving (Show, Read)

data Channel = Channel { _name   :: T.Text
                       , _videos :: [Video]
                       } deriving (Show, Read)



$(makeLenses ''Video)
$(makeLenses ''Channel)

getChannelURL :: String -> String
getChannelURL channelId = protocol ++ url' ++ generateSuffix [apiKey, channelId', part, order, limit]
  where
    -- Don't really care much if the api key is harcoded, feel free to stole it you bots
    apiKey         = ("key"        , "AIzaSyB945eCbGUpH-78zpkXSZmbbI29ZiRX094")
    protocol       = "https://"
    url'           = "www.googleapis.com/youtube/v3/search"
    channelId'     = ("channelId"  , channelId)
    part           = ("part"       , "snippet,id")
    order          = ("order"      , "date")
    limit          = ("maxResults" , "50")
    generateSuffix = ("?" ++) . drop 1 . foldl (\acc (k, val) -> acc ++ "&" ++ k ++ "=" ++ val) ""


parseVideo :: Maybe Value -> Maybe Video
parseVideo = parse
  where
    toSeconds timeStr =  T.pack . show . utSeconds $ parseUnixTime "%FT%X" (T.encodeUtf8 timeStr)
    format vidID = "https://www.youtube.com/v/" `T.append` vidID `T.append` "?vq=hd720"
    parse val = Video
                <$> val ^. key "snippet" . key "title"
                <*> (format <$> val ^. key "id" . key "videoId")
                <*> val ^. key "snippet" . key "thumbnails" . key "high" . key "url"
                <*> (toSeconds <$> val ^. key "snippet" . key "publishedAt")

decodeAPI :: BL.ByteString -> Maybe Channel
decodeAPI string = do
    let v = decode string ^. key "items"
    let title' =  v ^. nth 0 . key "snippet" . key "channelTitle"
    let vids = catMaybes $ v ^.. traverseArray . to parseVideo

    Channel <$> title' <*> return vids

fetch :: [String] -> IO [Channel]
fetch channelIds = do
    channels <- getPages decodeAPI (getChannelURL <$> channelIds)
    return . catMaybes $ join <$> channels

