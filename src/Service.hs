{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Service
                    -- ( Subscription, name, inputs, outputs, convert
                    -- , Provider(Youtube, Serie, Anime, Reddit, Forecast)
                    -- , onSub
                    -- , onSubM
                    -- , mkProvider
                    -- )
                    where

-- import qualified Eztv
import qualified ShowRss
import qualified Haruhichan
import qualified OpenWeather
import qualified Reddit
import qualified Youtube

import           Data.UnixTime

import Control.Applicative((<$>))

data Context input output = MkContext {
                lastUpdate :: UnixTime
               ,inputs     :: [input]
               ,outputs    :: [output]

            } deriving (Show)

data ServiceKind = Youtube | Serie | Anime | Reddit | Forecast | Any

data family Service (a :: ServiceKind)
data instance Service Youtube  = MkYoutube (Context String Youtube.Channel)      deriving (Show)
data instance Service Serie    = MkSerie (Context String ShowRss.Serie)             deriving (Show)
data instance Service Anime    = MkAnime (Context String Haruhichan.Anime)       deriving (Show)
data instance Service Reddit   = MkReddit (Context String Reddit.Reddit)         deriving (Show)
data instance Service Forecast = MkForecast (Context String OpenWeather.Weather) deriving (Show)
data instance Service Any      = YoutubeS (Service Youtube)
                               | SerieS (Service Serie)
                               | AnimeS (Service Anime)
                               | RedditS (Service Reddit)
                               | ForecastS (Service Forecast) deriving (Show)









class Provide (a :: ServiceKind) where
    name :: Service a -> String
    fetch :: Service a -> IO (Service a)

instance Provide Any where
    name (YoutubeS dat)  = name dat
    name (SerieS dat)    = name dat
    name (AnimeS dat)    = name dat
    name (RedditS dat)   = name dat
    name (ForecastS dat) = name dat

    fetch (YoutubeS dat)  = YoutubeS  <$> fetch dat
    fetch (SerieS dat)    = SerieS    <$> fetch dat
    fetch (AnimeS dat)    = AnimeS    <$> fetch dat
    fetch (RedditS dat)   = RedditS   <$> fetch dat
    fetch (ForecastS dat) = ForecastS <$> fetch dat

instance Provide Youtube where
    name _ = "youtube"
    fetch (MkYoutube ctx) = do
                out <- Youtube.fetch (inputs ctx)
                time <- getUnixTime
                return $ MkYoutube ctx { lastUpdate = time, outputs = out }

instance Provide Serie where
    name _ = "serie"
    fetch (MkSerie ctx) = do
                out <- ShowRss.fetch (inputs ctx)
                time <- getUnixTime
                return $ MkSerie ctx { lastUpdate = time, outputs = out }

instance Provide Anime where
    name _ = "anime"
    fetch (MkAnime ctx) = do
                out <- Haruhichan.fetch (inputs ctx)
                time <- getUnixTime
                return $ MkAnime ctx { lastUpdate = time, outputs = out }

instance Provide Reddit where
    name _ = "reddit"
    fetch (MkReddit ctx) = do
                out <- Reddit.fetch (inputs ctx)
                time <- getUnixTime
                return $ MkReddit ctx { lastUpdate = time, outputs = out }


instance Provide Forecast where
    name _ = "forecast"
    fetch (MkForecast ctx) = do
                out <- OpenWeather.fetch (inputs ctx)
                time <- getUnixTime
                return $ MkForecast ctx { lastUpdate = time, outputs = out }


