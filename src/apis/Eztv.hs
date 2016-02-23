{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Eztv ( Serie()
               , name, episodes, thumbnail
               , Episode()
               , title, magnetURI, date
               , fetch
               ) where

import           ClassyPrelude
import           Http                 (getPages)

import           Control.Lens
import           Text.HTML.TagSoup

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

data Episode = Episode { _title     :: !Text
                       , _magnetURI :: !Text
                       , _date      :: !Int
                       } deriving (Show)

data Serie = Serie { _name      :: !Text
                   , _thumbnail :: !Text
                   , _episodes  :: !(Vector Episode)
                   } deriving (Show)

$(makeLenses ''Episode)
$(makeLenses ''Serie)



extractData :: Text -> Maybe Serie
extractData xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let showname = extractTextFromTag "<span itemprop='name'>" tags
    let thumb = fromAttrib (T.pack "src") <$> find (~== "<img itemprop='image' src>") tags
    let items = filter (~== "<a class='magnet'>") tags
    let episodes' = mkEpisode <$> items

    Serie <$> showname
          <*> ((\l -> (T.pack baseUrl) <> l) <$> thumb)
          <*> return (fromList episodes')

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode tag = Episode (fst . T.breakOn (T.pack "Magnet") . fromAttrib (T.pack "title") $ tag)
                                (fromAttrib (T.pack "href") tag)
                                (3)

baseUrl :: String
baseUrl  = "https://eztv.ag"

craftUrl :: String -> String
craftUrl idx = baseUrl <> "/shows/" <> idx

fetch :: [String] -> IO [Serie]
fetch seriesIds = do
    series <- getPages (extractData . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    let !series' = catMaybes $ join <$> series
    return series'

