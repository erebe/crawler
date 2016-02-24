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
import Data.Char
import Prelude(read)
import Data.UnixTime

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


extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)

extractData :: UnixTime -> Text -> Maybe Serie
extractData time xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let showname = extractTextFromTag "<span itemprop='name'>" tags
    let thumb = fromAttrib (T.pack "src") <$> find (~== "<img itemprop='image' src>") tags
    let items = filter (~== "<a class='magnet'>") tags
    let episodes' = mkEpisode <$> zip items (getDates tags)

    Serie <$> showname
          <*> ((\l -> (T.pack baseUrl) <> l) <$> thumb)
          <*> return (fromList episodes')

    where
        getDates tags = catMaybes $ extractTextFromTag "<td class='forum_thread_post'>". snd 
                                 <$> (filter (\(ix, _) -> ix `mod` 5 == 0) 
                                             (zip ([1..]:: [Int]) (partitions (~== "<td class='forum_thread_post'>") tags)))

        mkEpisode (tag, date) = Episode (fst . T.breakOn (T.pack "Magnet") . fromAttrib (T.pack "title") $ tag)
                                (fromAttrib (T.pack "href") tag)
                                (fromEnum (utSeconds time) - (parseDate $ T.unpack date))

parseDate :: String -> Int
parseDate str = let (nb, rest) = span isDigit (dropWhile isSpace str) 
                in if null nb 
                   then 0
                   else let (coef, rest') = parseToken rest 
                        in ((read nb :: Int) * coef) + parseDate rest'
    where
        parseToken " week"  = (3600 * 24 * 7, "")
        parseToken " weeks" = (3600 * 24 * 7, "")
        parseToken " mo"    = (3600 * 24 * 30, "")
        parseToken " year"  = (3600 * 24 * 365, "")
        parseToken " years" = (3600 * 24 * 365, "")
        parseToken ('d':xs) = (3600 * 24, xs) 
        parseToken ('h':xs) = (3600, xs)
        parseToken ('m':_) = (60, "") 
        parseToken _ = (0, "") 

  

baseUrl :: String
baseUrl  = "https://eztv.ag"

craftUrl :: String -> String
craftUrl idx = baseUrl <> "/shows/" <> idx

fetch :: [String] -> IO [Serie]
fetch seriesIds = do
    time <- getUnixTime
    series <- getPages (extractData time . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    let !series' = catMaybes $ join <$> series
    return series'

