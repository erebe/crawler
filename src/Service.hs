
module Service where

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

data Service input output = MkService {
                lastUpdate :: UnixTime
               ,inputs     :: [input]
               ,outputs    :: [output]

            } deriving (Show)


newtype Youtube  = Youtube (Service String Youtube.Channel)
newtype Reddit   = Reddit (Service String Reddit.Reddit)
newtype Serie    = Serie (Service String ShowRss.Serie)
newtype Anime    = Anime (Service String Haruhichan.Anime)
newtype Forecast = Forecast (Service String OpenWeather.Weather)

data ServiceDTO = YoutubeDTO Youtube
                | RedditDTO Reddit
                | SerieDTO Serie
                | AnimeDTO Anime
                | ForecastDTO Forecast

mkYoutube :: [String] -> IO Youtube
mkYoutube ins = Youtube <$> fetch ins Youtube.fetch

mkReddit :: [String] -> IO Reddit
mkReddit ins = Reddit <$> fetch ins Reddit.fetch

mkSerie :: [String] -> IO Serie
mkSerie ins = Serie <$> fetch ins ShowRss.fetch

mkAnime :: [String] -> IO Anime
mkAnime ins = Anime <$> fetch ins Haruhichan.fetch

mkForecast :: [String] -> IO Forecast
mkForecast ins = Forecast <$> fetch ins OpenWeather.fetch

fetch :: [input] -> ([input] -> IO [out]) -> IO (Service input out)
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


