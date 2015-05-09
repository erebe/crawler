{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Haruhichan ( Episode()
                  , title, magnetURI, date
                  , Anime()
                  , name, thumbnail, episodes
                  , fetch
                  ) where


import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL


import           Data.Maybe

import           Control.Lens

import qualified Data.Text            as T

import           Control.Monad        (join)
import           Data.Aeson
import           Data.Aeson.Lens


data Episode = Episode { _title     :: T.Text
                       , _magnetURI :: T.Text
                       , _date      :: T.Text
                       } deriving (Show)


data Anime = Anime { _name      :: T.Text
                   , _thumbnail :: T.Text
                   , _episodes  :: [Episode]
                   } deriving (Show)

$(makeLenses ''Episode)
$(makeLenses ''Anime)

getAnimeURL :: String -> String
getAnimeURL animeId =  "http://ptp.haruhichan.com/anime.php?id=" ++ animeId


decodeAPI :: BL.ByteString -> Maybe Anime
decodeAPI js = decode js >>= \v ->
    Anime
      <$> v ^. key "name"
      <*> v ^. key "malimg"
      <*> return (parseEpisodes v)

    where
      parseEpisodes v = catMaybes $ v ^. key "episodes" ^.. traverseArray . to parseEpisode
      parseEpisode val = Episode
                         <$> val ^. key "name"
                         <*> val ^. key  "magnet"
                         <*> val ^. key "time"



fetch :: [String] -> IO [Anime]
fetch animeIds = do
    animes <- getPages decodeAPI (getAnimeURL <$> animeIds)

    return $ catMaybes (join <$> animes)

