{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Youtube where

import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BC

import           Control.Applicative
import           Data.Maybe

import           Control.Lens
import           GHC.Generics

import           Data.UnixTime

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(forM, join)

data Video = Video { _titre     :: T.Text
                    ,_url       :: String
                    ,_thumbnail :: String
                    ,_date      :: String

                   } deriving (Show, Read, Generic)

data Channel = Channel { _name   :: String
                        ,_videos :: [Video]

                       } deriving (Show, Read, Generic)



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
        format vidID = "https://www.youtube.com/v/" ++ vidID ++ "?vq=hd720"
        toSeconds timeStr =  show . utSeconds $ parseUnixTime (BC.pack "%FT%X") (BC.pack timeStr)

fetchChannels :: [String] -> IO [Channel]
fetchChannels channelsName = do
    youtubeVideos <- getPages decodeAPI (getChannelURL <$> channelsName)

    let channels =  flip fmap (zip channelsName youtubeVideos) $ \(chName, vids) ->
                       Channel chName (fromMaybe [] (join vids))

    return channels

