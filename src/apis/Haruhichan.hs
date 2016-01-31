{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Haruhichan ( Episode()
                  , title, magnetURI, date
                  , Anime()
                  , name, thumbnail, episodes
                  , fetch
                  ) where


import           ClassyPrelude
import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens

import           Control.DeepSeq


data Episode = Episode { _title     :: !Text
                       , _magnetURI :: !Text
                       , _date      :: !Text
                       } deriving (Show, Generic, NFData)


data Anime = Anime { _name      :: !Text
                   , _thumbnail :: !Text
                   , _episodes  :: !(Vector Episode)
                   } deriving (Show, Generic, NFData)

$(makeLenses ''Episode)
$(makeLenses ''Anime)

getAnimeURL :: String -> String
getAnimeURL animeId =  "http://ptp.haruhichan.com/anime.php?id=" <> animeId


decodeAPI :: BL.ByteString -> Maybe Anime
decodeAPI js = do
    v <- decode js :: Maybe Value
    Anime
      <$> v ^? key "name" . _String
      <*> v ^? key "malimg" . _String
      <*> (return . fromList) (parseEpisodes v)

    where
      parseEpisodes v = catMaybes $ v ^.. key "episodes" . _Array . traverse . to parseEpisode
      replaceUnderscore = T.map (\c -> if c == '_' then ' ' else c)
      parseEpisode val = Episode
                         <$> val ^? key "name" . _String . to replaceUnderscore
                         <*> val ^? key "magnet" . _String
                         <*> val ^? key "time" . _String


fetch :: [String] -> IO [Anime]
fetch animeIds = do
    animes <- getPages decodeAPI (getAnimeURL <$> animeIds)
    let !ff =  catMaybes $ join <$> animes
    return ff

