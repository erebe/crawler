{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenWeather ( Forecast(Forecast, date, temperature, description, iconUrl)
                   , Weather(Weather, city, forecasts)
                   , fetch
                   ) where


import           ClassyPrelude
import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens

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
                            ("http://openweathermap.org/img/w/" <> iconNb <> ".png")


buildWeatherApiUrl :: String -> String
buildWeatherApiUrl cityName =  "http://api.openweathermap.org/data/2.5/forecast/daily?q="
                               <> cityName <> "&mode=json&units=metric&cnt=7"



parseJsonApi :: BL.ByteString -> Maybe Weather
parseJsonApi js =  do
    v <- decode js :: Maybe Value
    Weather <$> v ^? key "city" . key "name" . _String
            <*> return (parseForecasts v)

  where
    parseForecasts v = catMaybes $ v ^.. key "list" . _Array . traverse . to parseForecast
    parseForecast v = mkForecast
                      <$> v ^? key "dt" . _Integral
                      <*> v ^? key "temp" . key "day" . _Double
                      <*> v ^? key "weather" . nth 0 . key "description" . _String
                      <*> v ^? key "weather" . nth 0 . key "icon" . _String

fetch :: [String] -> IO [Weather]
fetch cities = do
    pages <- getPages parseJsonApi (buildWeatherApiUrl <$> cities)

    return $ catMaybes (join <$> pages)
