{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Service
       -- ( mkYoutube
       --         , mkReddit
       --         , mkSerie
       --         , mkAnime
       --         , mkForecast
       --         , Service, lastUpdate, inputs, outputs
       --         , Youtube, Reddit, Serie, Anime, Forecast
       --         )
       where

-- import qualified Eztv
import qualified Haruhichan
import qualified OpenWeather
import qualified Reddit
import qualified ShowRss
import qualified Youtube

import           Data.Time
import           Data.UnixTime
import           System.Timeout

import           Control.Concurrent.Async (async, wait)

import           Control.Applicative      ((<$>), (<*>))
import           Control.Monad            (forM)
import           Data.Maybe               (catMaybes)

data ServiceKind =  YoutubeT | RedditT | SerieT | AnimeT | ForecastT
data Service (serviceType :: ServiceKind) input output = MkService {
                lastUpdate :: UnixTime
               ,inputs     :: [input]
               ,outputs    :: [output]

            } deriving (Show)


type Youtube  = Service YoutubeT String Youtube.Channel
type Reddit   = Service RedditT String Reddit.Reddit
type Serie    = Service SerieT String ShowRss.Serie
type Anime    = Service AnimeT String Haruhichan.Anime
type Forecast = Service ForecastT String OpenWeather.Weather

data ServiceDTO = YoutubeDTO Youtube
                | RedditDTO Reddit
                | SerieDTO Serie
                | AnimeDTO Anime
                | ForecastDTO Forecast

mkYoutube :: [String] -> IO Youtube
mkYoutube ins = fetch ins Youtube.fetch

mkReddit :: [String] -> IO Reddit
mkReddit ins = fetch ins Reddit.fetch

mkSerie :: [String] -> IO Serie
mkSerie ins = fetch ins ShowRss.fetch

mkAnime :: [String] -> IO Anime
mkAnime ins = fetch ins Haruhichan.fetch

mkForecast :: [String] -> IO Forecast
mkForecast ins = fetch ins OpenWeather.fetch

fetch :: [input] -> ([input] -> IO [out]) -> IO (Service t input out)
fetch ins fetcher = do
  out <- fetcher ins
  time <- getUnixTime
  return $ MkService time ins out


updateServices :: [IO ServiceDTO] -> IO [ServiceDTO]
updateServices services = do
  putStrLn "---------------------------------------------------"
  putStrLn . ("Start fetching :: " ++ ) . show =<< getLocalTime

  handles <- forM services (async . timeoutAfterMin 10)
  services' <- forM handles wait

  putStrLn . ("Done fetching :: " ++ ) . show =<< getLocalTime
  putStrLn "---------------------------------------------------"

  return $ catMaybes services'

  where
    getLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
    timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)


