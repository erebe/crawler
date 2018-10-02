{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GADTs   #-}

module HorribleSubs ( Episode()
                  , title, magnetURI, date
                  , Anime()
                  , name, thumbnail, episodes
                  , fetch
                  ) where


import           ClassyPrelude
import           Http                       (getPages)

import           Lens.Micro.TH
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match


import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Char
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UnixTime
import           Data.List                  (transpose)



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
    let thumb    = (\url -> if (T.pack "https://") `T.isPrefixOf` url then url else (T.pack "https://horriblesubs.info" <> url)) . fromAttrib (T.pack "src") <$> find (~== "<img class='img-responsive center-block'>") tags
    let showId   = find (\tag -> maybe False (T.isPrefixOf (T.pack "var hs_showid")) (maybeTagText tag)) tags
    let showNumber = takeWhile isDigit . dropWhile (not .isDigit) . T.unpack . fromTagText $ fromMaybe (TagText (T.pack "")) showId

    now <- getUnixTime
    today <- BL.fromStrict <$> formatUnixTime (BC.pack "%D") now
    yesterday <- BL.fromStrict <$> formatUnixTime (BC.pack "%D") (addUnixDiffTime now (secondsToUnixDiffTime (3600 * 24)))


    episodes' <- do
      items <- getPages (parseTagsOptions parseOptionsFast) [apiUrl showNumber]
      let tags' = fromMaybe mempty (headMay $ catMaybes items)
      let items_720 = partitions (~== "<div class='rls-link link-720p'>") tags'
      let items_1080 = partitions (~== "<div class='rls-link link-1080p'>") tags'
      let datesRaws = innerText . getTagContent (BLC.pack "span") (anyAttrValueLit (BLC.pack "rls-date"))  <$> partitions (~== "<span class='rls-date'>") tags'
      let normalizedDates = (\date -> if date == (BLC.pack "Yesterday") then yesterday else if date == (BLC.pack "Today") then today else date) <$> datesRaws
      let dates =  parseUnixTime (BC.pack "%D") . BL.toStrict . BLC.takeWhile (/= '(') . BLC.dropWhile (== '(') <$> normalizedDates
      return $ concat $ (transpose [mkEpisode <$> zip items_1080 dates, mkEpisode <$> zip items_720 dates])


    return $ Anime <$> showname
                   <*> thumb
                   <*> return (fromList episodes')

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode (tag,datee) = Episode (T.drop 2 . T.dropWhile (/= ']') . T.decodeUtf8 . BL.toStrict . fromMaybe mempty $ fromAttrib (BLC.pack "href") <$> find (~== "<a title='XDCC search'>") tag)
                                        (T.decodeUtf8 . BL.toStrict . fromMaybe mempty $ fromAttrib (BLC.pack "href") <$> find (~== "<a title='Magnet Link'>") tag)
                                     datee

baseUrl :: String
baseUrl = "https://horriblesubs.info/shows/"

craftUrl :: String -> String
craftUrl animeName = baseUrl <> animeName


apiUrl :: String -> String
apiUrl showId = "https://horriblesubs.info/api.php?method=getshows&type=show&showid=" <> showId

fetch :: [String] -> IO [Anime]
fetch seriesIds = do
    time <- getUnixTime
    series <- getPages (extractData time . T.decodeUtf8 . BL.toStrict) (craftUrl <$> seriesIds)

    series' <- sequence (catMaybes series)
    return $ catMaybes series'
