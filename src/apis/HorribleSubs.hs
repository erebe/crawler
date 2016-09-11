{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module HorribleSubs ( Episode()
                  , title, magnetURI, date
                  , Anime()
                  , name, thumbnail, episodes
                  , fetch
                  ) where


import           ClassyPrelude
import           Http                       (getPages)

import           Control.Lens               hiding (ix)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match


import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Char
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UnixTime



data Episode = Episode { _title     :: !Text
                       , _magnetURI :: !Text
                       , _date      :: !UnixTime
                       } deriving (Show)


data Anime = Anime { _name      :: !Text
                   , _thumbnail :: !Text
                   , _episodes  :: !(Vector Episode)
                   } deriving (Show)


$(makeLenses ''Episode)
$(makeLenses ''Anime)


extractData :: UnixTime -> Text -> IO (Maybe Anime)
extractData _ xmlStr = do
    let tags     = parseTagsOptions parseOptionsFast xmlStr
    let showname = extractTextFromTag "<h1 class='entry-title'>" tags
    let thumb    = fromAttrib (T.pack "src") <$> find (~== "<img class='size-full alignleft'>") tags
    let showId   = find (\tag -> maybe False (T.isPrefixOf (T.pack "var hs_showid")) (maybeTagText tag)) tags

    episodes' <- flip (maybe (return mempty)) showId $ \showId' -> do
      let showNumber = takeWhile isDigit . dropWhile (not .isDigit) . T.unpack . fromTagText $ showId'
      items <- getPages (parseTagsOptions parseOptionsFast) [apiUrl showNumber]
      let tags' = fromMaybe mempty (headMay $ catMaybes items)
      let items' = partitions (tagOpen (== BLC.pack "tr") null) tags'
      let datesRaws = innerText . getTagContent (BLC.pack "td") (anyAttrValueLit (BLC.pack "rls-label"))  <$> partitions (~== "<td class='rls-label'>") tags'
      let dates =  parseUnixTime (BC.pack "%D") . BL.toStrict . BLC.takeWhile (/= '(') . BLC.dropWhile (== '(') <$> datesRaws
      let datesList = foldMap (replicate 3) dates :: [UnixTime]
      return $ mkEpisode <$> zip items' datesList


    return $ Anime <$> showname
                   <*> thumb
                   <*> return (fromList episodes')

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode (tag,datee) = Episode (T.decodeUtf8 . BL.toStrict . innerText . getTagContent (BLC.pack "td") (anyAttrValueLit (BLC.pack "dl-label")) $ tag)
                                     (T.decodeUtf8 . BL.toStrict . fromMaybe mempty $ fromAttrib (BLC.pack "href") <$> find (~== "<a title='Magnet Link'>") tag)
                                     datee

baseUrl :: String
baseUrl = "http://horriblesubs.info/shows/"

craftUrl :: String -> String
craftUrl animeName = baseUrl <> animeName


apiUrl :: String -> String
apiUrl showId = "http://horriblesubs.info/lib/getshows.php?type=show&showid=" <> showId <> "&nextid=0"

fetch :: [String] -> IO [Anime]
fetch seriesIds = do
    time <- getUnixTime
    series <- getPages (extractData time . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    series' <- sequence (catMaybes series)
    return $ catMaybes series'
