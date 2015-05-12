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
getAnimeURL animeId =  "http://ptp.haruhichan.com/anime.php?id=" <> animeId


decodeAPI :: BL.ByteString -> Maybe Anime
decodeAPI js = do
    v <- decode js :: Maybe Value
    Anime
      <$> v ^? key "name" . _String
      <*> v ^? key "malimg" . _String
      <*> return (parseEpisodes v)

    where
      parseEpisodes v = catMaybes $ v ^.. key "episodes" . _Array . traverse . to parseEpisode
      parseEpisode val = Episode
                         <$> val ^? key "name" . _String
                         <*> val ^? key "magnet" . _String
                         <*> val ^? key "time" . _String


fetch :: [String] -> IO [Anime]
fetch animeIds = do
    animes <- getPages decodeAPI (getAnimeURL <$> animeIds)

    return $ catMaybes (join <$> animes)

