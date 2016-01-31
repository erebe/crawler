{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module ShowRss ( Serie()
               , name, episodes
               , Episode()
               , title, magnetURI, date
               , fetch
               ) where

import           ClassyPrelude
import           Http                 (getPages)

import           Control.Lens
import           Data.UnixTime
import           Text.HTML.TagSoup

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Control.DeepSeq

data Episode = Episode { _title     :: !Text
                       , _magnetURI :: !Text
                       , _date      :: !Int
                       } deriving (Show, Generic, NFData)

data Serie = Serie { _name     :: !Text
                   , _episodes :: !(Vector Episode)
                   } deriving (Show, Generic, NFData)

$(makeLenses ''Episode)
$(makeLenses ''Serie)



extractData :: Text -> Maybe Serie
extractData xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let showname =  T.drop (length title_suffix) <$> extractTextFromTag "<title>" tags
    let items = partitions (~== "<item>") tags
    let episodes' = catMaybes $ mkEpisode <$> items
    Serie <$> showname <*> return (fromList episodes')

    where
        title_suffix = "showRSS: feed for "
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode tags = Episode <$> extractTextFromTag "<title>" tags
                                 <*> (fromAttrib (T.pack "url") <$> find (~== "<enclosure>") tags)
                                 <*> (fromEnum . utSeconds . parseUnixTime webDateFormat . T.encodeUtf8
                                      <$> extractTextFromTag "<pubDate>" tags )


craftUrl :: String -> String
craftUrl idx = protocol <> baseUrl <> idx <> extension
    where
        protocol  = "http://"
        baseUrl   = "showrss.info/feeds/"
        extension = ".rss"

fetch :: [String] -> IO [Serie]
fetch seriesIds = do
    series <- getPages (extractData . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    let !series' = force . catMaybes $ join <$> series
    return series'


