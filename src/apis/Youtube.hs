{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Youtube ( Video()
               , videos, name
               , Channel()
               , title, url, thumbnail, date
               , fetch
               ) where

import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL


import           Data.Maybe

import           Control.Lens

import           Data.UnixTime

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(forM, join)

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
getChannelURL str = "https://gdata.youtube.com/feeds/api/users/" ++ str ++ "/uploads?v=2&alt=jsonc&ordered=published"



decodeAPI :: BL.ByteString -> Maybe [Video]
decodeAPI string = do
    result <- decode string
    join $ flip parseMaybe result $ \obj -> do
             dataa <- obj .: "data"
             items <- dataa .: "items" :: Parser [Object]
             return $ forM items $ \jitem ->
                    flip parseMaybe jitem $ \item ->
                        Video <$> item .: "title"
                              <*> (format <$> (item .: "id"))
                              <*> (item .: "thumbnail" >>= (.: "hqDefault"))
                              <*> (toSeconds <$> (item .: "updated"))

    where
        format vidID = "https://www.youtube.com/v/" `T.append` vidID `T.append` "?vq=hd720"
        toSeconds timeStr =  T.pack . show . utSeconds $ parseUnixTime "%FT%X" (T.encodeUtf8 timeStr)

fetch :: [String] -> IO [Channel]
fetch channelsName = do
    youtubeVideos <- getPages decodeAPI (getChannelURL <$> channelsName)

    let channels = zipWith (\chName vids -> Channel (T.pack chName) (fromMaybe [] (join vids)))
                           channelsName youtubeVideos

    return channels

