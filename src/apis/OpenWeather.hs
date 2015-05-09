{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module OpenWeather ( Forecast(Forecast, date, temperature, description, iconUrl)
                   , Weather(Weather, city, forecasts)
                   , fetch
                   ) where



import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Control.Lens
import           Control.Monad        (join)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Int
import           Data.Maybe
import           Data.UnixTime
import           Foreign.C.Types

data Forecast = Forecast { date        :: T.Text
                         , temperature :: !Double
                         , description :: T.Text
                         , iconUrl     :: T.Text
                         } deriving (Show)

data Weather = Weather { city      :: T.Text
                       , forecasts :: [Forecast]
                       }  deriving (Show)


mkForecast :: Int64 -> Double -> T.Text -> T.Text -> Forecast
mkForecast epoch temp desc iconNb = Forecast
                            (T.decodeUtf8 $ formatUnixTimeGMT "%A %d %B" $ UnixTime (CTime epoch) 0)
                            temp
                            desc
                            ("http://openweathermap.org/img/w/" `T.append` iconNb `T.append` ".png")


buildWeatherApiUrl :: String -> String
buildWeatherApiUrl cityName =  "http://api.openweathermap.org/data/2.5/forecast/daily?q=" ++ cityName ++ "&mode=json&units=metric&cnt=7"



parseJsonApi :: BL.ByteString -> Maybe Weather
parseJsonApi js =  do
    v <- decode js
    Weather <$> v ^. key "city" . key "name"
            <*> return (parseForecasts v)

  where
    parseForecasts v = catMaybes $ v ^.. key "list" . traverseArray . to parseForecast
    parseForecast v = mkForecast
                      <$> v ^. key "dt"
                      <*> v ^. key "temp" . key "day"
                      <*> v ^. key "weather" . nth 0 . key "description"
                      <*> v ^. key "weather" . nth 0 . key "icon"

fetch :: [String] -> IO [Weather]
fetch cities = do
    pages <- getPages parseJsonApi (buildWeatherApiUrl <$> cities)

    return $ catMaybes (join <$> pages)
