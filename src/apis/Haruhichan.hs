{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Haruhichan ( Episode()
                  , title, magnetURI, date
                  , Anime()
                  , name, thumbnail, episodes
                  , fetchAnimes
                  ) where


import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL

import           Control.Applicative
import           Data.Maybe

import           Control.Lens

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(forM, join)


data Episode = Episode { _title :: T.Text
                        ,_magnetURI :: String
                        ,_date :: String

                       } deriving (Show)


data Anime = Anime { _name      :: String
                    , _thumbnail :: String
                    , _episodes  :: [Episode]

                   } deriving (Show)

$(makeLenses ''Episode)
$(makeLenses ''Anime)

getAnimeURL :: String -> String
getAnimeURL animeId =  "http://ptp.haruhichan.com/anime.php?id=" ++ animeId


decodeAPI :: BL.ByteString -> Maybe Anime
decodeAPI js = do
    result <- decode js
    join $ flip parseMaybe result $ \obj -> do
        jsName <- obj .: "name"
        jsThumbnail <- obj .: "malimg"
        episodesObj <- obj .: "episodes" :: Parser [Object]
        episodes' <- extractEpisodes episodesObj

        return $ Anime <$> jsName
                       <*> jsThumbnail
                       <*> episodes'

    where
        extractEpisodes objs = return $ forM objs $ \episodeObj ->
            flip parseMaybe episodeObj $ \episode ->
                Episode <$> episode .: "name"
                        <*> episode .: "magnet"
                        <*> episode .: "time"



fetchAnimes :: [String] -> IO [Anime]
fetchAnimes animeIds = do
    animes <- getPages decodeAPI (getAnimeURL <$> animeIds)

    return $ catMaybes (join <$> animes)

