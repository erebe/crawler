{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Youtube ( Video()
               , videos, name
               , Channel()
               , title, url, thumbnail, date
               , fetch
               ) where

import           Http                 (getPages)

import           ClassyPrelude
import           Data.UnixTime

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens


data Video = Video { _title     :: !Text
                   , _url       :: !Text
                   , _thumbnail :: !Text
                   , _date      :: !Text
                   } deriving (Show)

data Channel = Channel { _name   :: !Text
                       , _videos :: !(Vector Video)
                       } deriving (Show)



$(makeLenses ''Video)
$(makeLenses ''Channel)

getChannelURL :: String -> String
getChannelURL channelId = protocol <> url' <> generateSuffix [apiKey, channelId', part, order, limit]
  where
    -- Don't really care much if the api key is harcoded, feel free to stole it you bots
    apiKey         = ("key"        , "AIzaSyB945eCbGUpH-78zpkXSZmbbI29ZiRX094")
    protocol       = "https://"
    url'           = "www.googleapis.com/youtube/v3/search"
    channelId'     = ("channelId"  , channelId)
    part           = ("part"       , "snippet,id")
    order          = ("order"      , "date")
    limit          = ("maxResults" , "50")
    generateSuffix = ("?" <>) . drop 1 . foldMap (\(k, val) -> "&" <> k <> "=" <> val)


parseVideo :: Value -> Maybe Video
parseVideo = parse
  where
    seconds timeStr =  T.pack . show . utSeconds $ parseUnixTime "%FT%X" (T.encodeUtf8 timeStr)
    format vidID = "https://www.youtube.com/embed/" <> vidID <> "?hd=1&vq=hd720"
    parse val = Video
                <$> val ^? key "snippet" . key "title" . _String
                <*> val ^? key "id" . key "videoId" . _String . to format
                <*> val ^? key "snippet" . key "thumbnails" . key "high" . key "url" . _String
                <*> val ^? key "snippet" . key "publishedAt" . _String . to seconds

decodeAPI :: BL.ByteString -> Maybe Channel
decodeAPI string = do
    v <- (decode string  :: Maybe Value) >>= firstOf (key "items")
    let title' =  v ^? nth 0 . key "snippet" . key "channelTitle" . _String
    let vids = catMaybes $ v ^.. _Array . traverse . to parseVideo

    Channel <$> title' <*> return (fromList vids)

fetch :: [String] -> IO [Channel]
fetch channelIds = do
    channels <- getPages decodeAPI (getChannelURL <$> channelIds)
    let !channels' = catMaybes $ join <$> channels
    return channels'
