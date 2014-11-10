{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}


module OpenWeather ( Forecast(Forecast, date, temperature, description, iconUrl)
                   , Weather(Weather, city, forecasts)
                   , fetchWeathers
                   ) where

import           Control.Applicative

import           Http(getPages)
import           GHC.Generics

import qualified Data.ByteString.Lazy       as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(join, mzero, forM)
import Data.Maybe
import Data.UnixTime
import Data.Int
import Foreign.C.Types

data Forecast = Forecast { date :: T.Text
                         , temperature :: !Double
                         , description :: T.Text
                         , iconUrl :: T.Text
                         } deriving (Show, Generic)

data Weather = Weather { city :: T.Text
                       , forecasts :: [Forecast]
                       }  deriving (Show, Generic)


mkForecast :: Int64 -> Double -> T.Text -> T.Text -> Forecast
mkForecast epoch temp desc iconNb = Forecast
                            (T.decodeUtf8 $ formatUnixTimeGMT "%A %d %B" $ UnixTime (CTime epoch) 0)
                            temp
                            desc
                            ("http://openweathermap.org/img/w/" `T.append` iconNb `T.append` ".png")


buildWeatherApiUrl :: String -> String
buildWeatherApiUrl cityName =  "http://api.openweathermap.org/data/2.5/forecast/daily?q=" ++ cityName ++ "&mode=json&units=metric&cnt=7"



parseJsonApi :: BL.ByteString -> Maybe Weather
parseJsonApi jsonStr =  do
    jsonObj <- decode jsonStr
    flip parseMaybe jsonObj $ \obj -> do
        cityName   <- obj .: "city" >>= (.: "name")
        forecasts' <- obj .: "list" :: Parser [Object]

        t <- forM forecasts' $ \forecast -> do
                weather <- listToMaybe <$> (forecast .: "weather" :: Parser [Object])
                case weather of
                    Just weather' -> mkForecast <$> forecast .: "dt"
                                                <*> (forecast .: "temp" >>= (.: "day"))
                                                <*> weather' .: "description"
                                                <*> weather' .: "icon"
                    _ -> mzero

        return $ Weather cityName t


fetchWeathers :: [String] -> IO [Weather]
fetchWeathers cities = do
    pages <- getPages parseJsonApi (buildWeatherApiUrl <$> cities)

    return $ catMaybes (join <$> pages)
