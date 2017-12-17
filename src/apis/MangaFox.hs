{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module MangaFox ( Manga()
               , name, chapters, thumbnail
               , Chapter()
               , title, url, date
               , fetch
               ) where

import           ClassyPrelude
import           Http                 (getPages)

import           Lens.Micro.TH
import           Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.UnixTime

data Chapter = Chapter { _title     :: !Text
                       , _url       :: !Text
                       , _date      :: !Int
                       } deriving (Show, Generic, NFData)

data Manga = Manga { _name      :: !Text
                   , _thumbnail :: !Text
                   , _chapters  :: !(Vector Chapter)
                   } deriving (Show, Generic, NFData)

$(makeLenses ''Chapter)
$(makeLenses ''Manga)



extractData :: UnixTime -> Text -> Maybe Manga
extractData time xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let showname = T.strip . T.takeWhile (/= '(') <$> extractTextFromTag "<a style='color:#666' href>" tags
    let thumb = fromAttrib (T.pack "src") <$> find (~== "<img alt onerror src>") tags
    let items = filter (~== "<a class='tips' href>") tags
    let episodes' = mkChapter <$> zip items (getDates tags)

    Manga <$> showname
          <*> thumb
          <*> return (fromList episodes')

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        getDates tags = catMaybes $ extractTextFromTag "<span class='date'>"
                                 <$> partitions (~== "<span class='date'>") tags

        mkChapter (tag, dateStr) = Chapter
            (T.strip . foldMap id . reverse. take 1 . drop 1 .reverse $ T.split (== '/') (fromAttrib (T.pack "href") tag))
            (T.pack "http:" <> fromAttrib (T.pack "href") tag)

            (fromEnum . utSeconds $ parseDate time (T.unpack dateStr))

parseDate :: UnixTime -> String -> UnixTime
parseDate now dateStr =
  let date = parseUnixTime (B.pack "%b %d, %Y") (B.pack dateStr) -- shortMonth day, Year
   in if fromEnum (utSeconds date) /= -2209075761 -- invalid date
      then date
      else addUnixDiffTime now (parseRelativeDate dateStr)
                   -- else let (coef, rest') = parseToken rest
                   --      in ((read nb :: Int) * coef) + parseDate rest'
    where
      parseRelativeDate "Yesterday" = secondsToUnixDiffTime (-3600 * 24 :: Int)
      parseRelativeDate "Today" = secondsToUnixDiffTime (-3600 * 12 :: Int)
      parseRelativeDate str = let hoursAgo = fromMaybe 0 . readMay . fst $ span isDigit str :: Int
                               in secondsToUnixDiffTime (-3600 * hoursAgo)


baseUrl :: String
baseUrl = "http://mangafox.la/manga/"

craftUrl :: String -> String
craftUrl idx = baseUrl <> idx

fetch :: [String] -> IO [Manga]
fetch seriesIds = do
    time <- getUnixTime
    series <- getPages (extractData time . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    let !series' = force . catMaybes $ join <$> series
    return series'

