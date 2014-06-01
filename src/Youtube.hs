{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Youtube where

import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BC

import           Control.Applicative
import           Data.Maybe

import           Control.Lens
import           Control.Arrow((&&&), (>>>))
import qualified Text.XML.Light as XML
import           GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Video = Video { _titre     :: T.Text
                    ,_url       :: String
                    ,_thumbnail :: String

                   } deriving (Show, Read, Generic)

data Channel = Channel { _name   :: String
                        ,_videos :: [Video]

                       } deriving (Show, Read, Generic)

$(makeLenses ''Video)
$(makeLenses ''Channel)


getChannelURL :: String -> String
getChannelURL str = "http://www.youtube.com/user/" ++ str ++ "/videos"



toVideo :: String -> String -> Video
toVideo a b = Video T.empty "" ""
              & titre     .~ (T.decodeUtf8 . BC.pack $ b)
              & url       .~ "http://www.youtube.com/watch?v=" ++ a
              & thumbnail .~ "http://i1.ytimg.com/vi/" ++ a ++ "/mqdefault.jpg"



extractVideos :: BL.ByteString -> [Video]
extractVideos htmlPage =
       concat $ ((\t -> extractUUID <$> filterUUIDEls t) &&& (\el -> extractTitle <$> filterTitleEls el)
                    >>> (\(uids,titles) -> ((\(uid, title) -> toVideo (fromMaybe "" uid)  (fromMaybe "" title)) <$> zip uids titles))
                ) <$> parseXml htmlPage

    where
        parseXml        = XML.onlyElems . XML.parseXML
        uidStr          = "data-video-ids"
        extractUUID     = XML.findAttr (XML.QName uidStr Nothing Nothing)
        extractTitle    = XML.findAttr (XML.QName "title" Nothing Nothing)
        filterUUIDEls   = XML.filterElements (isJust . XML.findAttr (XML.QName uidStr Nothing Nothing))
        -- LOL
        filterTitleEls ll = fromMaybe []
                                 $ XML.filterElements (\el -> (XML.qName (XML.elName el) == "a")
                                                           && isJust (XML.findAttr (XML.QName "data-sessionlink" Nothing Nothing) el)
                                                           && isJust (XML.findAttr (XML.QName "title" Nothing Nothing) el))
                                <$> XML.filterElement (\el -> fromMaybe "" (XML.findAttr (XML.QName "id" Nothing Nothing) el) == "video-page-content") ll


fetchChannels :: [String] -> IO [Channel]
fetchChannels channelsName = do
        youtubeVideos <- getPages (return . extractVideos) (getChannelURL <$> channelsName)
        let vids = (\(chName, xs) -> Channel "" []
                                     & name   .~ chName
                                     & videos .~ fromMaybe [] xs

                   ) <$> zip channelsName youtubeVideos

        return vids

