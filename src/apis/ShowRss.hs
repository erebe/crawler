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

data Episode = Episode { _title     :: T.Text
                       , _magnetURI :: T.Text
                       , _date      :: UnixTime
                       } deriving (Show)

data Serie = Serie { _name     :: T.Text
                   , _episodes :: [Episode]
                   } deriving (Show)

$(makeLenses ''Episode)
$(makeLenses ''Serie)



extractData :: T.Text -> Maybe Serie
extractData xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let showname =  T.drop (length title_suffix) <$> extractTextFromTag "<title>" tags
    let items = partitions (~== "<item>") tags
    let episodes' = catMaybes $ mkEpisode <$> items
    Serie <$> showname <*> return episodes'

    where
        title_suffix = "showRSS: feed for "
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode tags = Episode <$> extractTextFromTag "<title>" tags
                                 <*> (fromAttrib (T.pack "url") <$> find (~== "<enclosure>") tags)
                                 <*> (parseUnixTime webDateFormat . T.encodeUtf8 <$> extractTextFromTag "<pubDate>" tags )


craftUrl :: String -> String
craftUrl idx = protocol <> baseUrl <> idx <> extension
    where
        protocol  = "http://"
        baseUrl   = "showrss.info/feeds/"
        extension = ".rss"

fetch :: [String] -> IO [Serie]
fetch seriesIds = do
    series <- getPages (extractData . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    return $ catMaybes $ join <$> series


