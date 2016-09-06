{-# LANGUAGE BangPatterns      #-}
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
import           Http                 (getPages)

import           Control.Lens         hiding (ix)
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match

import           Control.DeepSeq
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text            as T
import Data.Char
import qualified Data.Text.Encoding   as T
import           Data.UnixTime



data Episode = Episode { _title     :: !Text
                       , _magnetURI :: !Text
                       , _date      :: !Int
                       } deriving (Show, Generic, NFData)


data Anime = Anime { _name      :: !Text
                   , _thumbnail :: !Text
                   , _episodes  :: !(Vector Episode)
                   } deriving (Show, Generic, NFData)


$(makeLenses ''Episode)
$(makeLenses ''Anime)


extractData :: UnixTime -> Text -> IO (Maybe Anime)
extractData time xmlStr = do
    let tags     = parseTagsOptions parseOptionsFast xmlStr
    let showname = extractTextFromTag "<h1 class='entry-title'>" tags
    let thumb    = fromAttrib (T.pack "src") <$> find (~== "<img class='size-full alignleft'>") tags
    let showId   = find (\tag -> maybe False (T.isPrefixOf (T.pack "var hs_showid")) (maybeTagText tag)) tags

    episodes' <- flip (maybe (return mempty)) showId $ \showId' -> do
      let showNumber = takeWhile isDigit . dropWhile (not .isDigit) . T.unpack . fromTagText $ showId'
      items <- getPages (parseTagsOptions parseOptionsFast) [apiUrl showNumber]
      let items'   = partitions (tagOpen (== (BLC.pack "tr")) null) (fromMaybe mempty (headMay $ catMaybes items))
      return $ mkEpisode <$> items'


    return $ Anime <$> showname
                   <*> thumb
                   <*> return (fromList episodes')

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode tag = Episode  (T.decodeUtf8 . BL.toStrict . innerText . getTagContent (BLC.pack "td") (anyAttrValueLit (BLC.pack "dl-label")) $ tag)
                                (T.decodeUtf8 . BL.toStrict . fromMaybe mempty $ fromAttrib (BLC.pack "href") <$> find (~== "<a title='Magnet Link'>") tag)
                                 0

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
    return . force $ force <$> catMaybes series'
