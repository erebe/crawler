{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Eztv ( Serie()
            , name, episodes
            , Episode()
            , title, magnetURI, date, link, fileName
            , fetchSeries
            ) where

import           Http(getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Text.HTML.TagSoup
import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import           Data.UnixTime

import           Control.Lens
import           GHC.Generics
import           Data.List

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Episode = Episode { _title      :: T.Text
                        ,_magnetURI :: String
                        ,_date      :: UnixTime
                        ,_link      :: String
                        ,_fileName  :: String

                        } deriving (Show, Generic)

data Serie = Serie { _name :: String
                    ,_episodes  :: [Episode]

                   } deriving (Show, Generic)

$(makeLenses ''Episode)
$(makeLenses ''Serie)



extractData :: BL.ByteString -> [Episode]
extractData xmlStr = do
    let tags = parseTagsOptions parseOptionsFast xmlStr
    let items = partitions (~== "<item>") tags
    catMaybes $ mkEpisode <$> items

    where
        extractTextFromTag tag tags = maybeTagText =<< (listToMaybe . drop 1 $ dropWhile (~/= tag) tags)
        mkEpisode tags = Episode <$> (T.decodeUtf8 . BL.toStrict <$> extractTextFromTag "<title>" tags)
                                 <*> (BLC.unpack <$> extractTextFromTag "<magnetURI>" tags )
                                 <*> (parseUnixTime webDateFormat . BL.toStrict <$> extractTextFromTag "<pubDate>" tags )
                                 <*> (BLC.unpack . fromAttrib (BLC.pack "url") <$> find (~== "<enclosure>") tags)
                                 <*> (BLC.unpack <$> extractTextFromTag "<fileName>" tags)


craftEzrssUrl :: String -> String
craftEzrssUrl names = protocol ++ baseUrl ++ buildArgs
    where
        protocol  = "http://"
        baseUrl   = "ezrss.it/search/index.php"
        args      = [ ("simple", "")
                     ,("mode", "rss")
                     ,("show_name_exact", "true")
                     ,("show_name", names)
                    ]
        buildArgs = '?' : ( drop 1 . join $
                            (\(x, y) -> "&" ++ x ++ "=" ++ y ) <$> args
                          )

fetchSeries :: [String] -> IO [Serie]
fetchSeries seriesNames = do
    episodes' <- getPages extractData (craftEzrssUrl <$> seriesNames)
    let series = (\(nameS, eps) -> name .~ nameS
                                 $ episodes .~ fromMaybe [] eps
                                 $ Serie "" []
                 )
                <$> zip seriesNames episodes'

    return series


