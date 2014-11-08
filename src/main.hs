{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

import qualified Eztv
import qualified Youtube
import qualified Weather

import           Control.Applicative             ((<$>), (<*>))
import           Control.Concurrent.Async        (async, wait)
import           Control.Concurrent.MVar         (MVar, newMVar, readMVar, swapMVar)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad                   (forever, guard, forM)
import           Data.Maybe                      (fromMaybe)
import           System.Directory                (getHomeDirectory, doesFileExist)

import           Control.Lens                    hiding ((.=))
import           Control.Monad.IO.Class          (liftIO)
import           Web.Scotty
import           Network.HTTP.Types.Status       (ok200)

import qualified Data.Text as T

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           Data.List

import           Data.Time
import           System.Timeout

import           Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

instance ToJSON Weather.Forecast
instance ToJSON Weather.Weather
instance ToJSON Eztv.Episode
instance ToJSON Eztv.Serie
instance ToJSON Youtube.Video
instance ToJSON Youtube.Channel

newtype Configuration = Configuration [(String, [String])]

getConfig :: Configuration -> String -> [String]
getConfig (Configuration cfg) name = fromMaybe [] $ lookup name cfg


data Service = Video | Meteo | Serie | SMS | Unknown
               deriving (Eq)

data ServiceData = VideoData [Youtube.Channel]
                 | MeteoData [Weather.Weather]
                 | SerieData [Eztv.Serie]
                 | SMSData [Int]
                 | None

servicesMap :: [(Service, (String, [String] -> IO ServiceData))]
servicesMap =  [ (Video,  ("video", \args -> VideoData <$> Youtube.fetchChannels args))
               , (Meteo,  ("meteo", \args -> MeteoData <$> Weather.fetchWeathers args))
               , (Serie,  ("serie", \args -> SerieData <$> Eztv.fetchSeries args))
               , (SMS,    ("sms",   \_ -> return None))
               ]

instance Show Service where
    show service = fromMaybe "Unknown" $ fst
                   <$> lookup service servicesMap

strToService :: String -> Service
strToService str = fromMaybe Unknown $ fst
                   <$> find ((== str) . fst . snd) servicesMap


fetchServiceData :: Service -> [String] -> IO ServiceData
fetchServiceData service args = case fetcher of
                                    Just fetcher' -> fetcher' args
                                    _             -> return None
    where
        fetcher = snd <$> lookup service servicesMap


class ServiceAction a where
    listA :: a -> Maybe (ActionM ())
    lastA :: a -> Maybe (ActionM ())
    findA :: String -> a ->  Maybe (ActionM ())
    dispatch :: String -> a -> Maybe (ActionM ())

instance ServiceAction ServiceData where
    listA (SerieData series)   = Just $ raw . encodePretty $ series^..traverse.Eztv.serieName
    listA (VideoData channels) = Just $ raw . encodePretty $ channels^..traverse.Youtube.name
    listA (MeteoData cities)   = Just $ raw . encodePretty $ Weather.city <$> cities
    listA _                    = Nothing

    lastA (SerieData series)   = Just $ raw . encodePretty $ [serie & Eztv.episodes .~ take 1 (serie^.Eztv.episodes) | serie <- series]
    lastA (VideoData channels) = Just $ raw . encodePretty $ [channel & Youtube.videos .~ take 1 (channel^.Youtube.videos) | channel <- channels]
    lastA (MeteoData cities)   = Just $ raw . encodePretty $ [Weather.Weather (Weather.city city) (take 1 $ Weather.forecasts city) | city <- cities]
    lastA _                    = Nothing


    findA toFind (SerieData series)   = Just $ raw . encodePretty $ series^..traversed.filtered (\serie -> toFind `isInfixOf` (serie^.Eztv.serieName))
    findA toFind (VideoData channels) = Just $ raw . encodePretty $ channels^..traversed.filtered (\channel -> toFind `isInfixOf` (channel^.Youtube.name))
    findA toFind (MeteoData cities)   = Just $ raw . encodePretty $ [ city | city <- cities, T.toLower (T.pack toFind)
                                                                                             `T.isInfixOf`
                                                                                             T.toLower (Weather.city city)]
    findA _ _                         = Nothing

    dispatch arg | "list" <- arg = listA
                 | "last" <- arg = lastA
                 | ""     <- arg = lastA
                 | otherwise = findA arg





loadConfigFile :: IO Configuration
loadConfigFile = do
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return $ Configuration config

        Nothing     -> do
                       putStrLn "############################################################"
                       putStrLn "#############       Config file not found     ##############"
                       putStrLn "######### Please add one at ~/.config/crawler.rc  ##########"
                       putStrLn "############################################################"
                       return $ Configuration []

    where
        extractConfig :: MaybeT IO [(String, [String])]
        extractConfig = do
            configPath      <- liftIO $ (++ "/.config/crawler.rc") <$> getHomeDirectory
            isConfigPresent <- liftIO $ doesFileExist configPath
            guard isConfigPresent

            liftIO $ read <$> readFile configPath


spawnFetcher :: IO (MVar [(Service, ServiceData)])
spawnFetcher = do
        fetchRes <- newMVar []
        _ <- async $ fetcher fetchRes
        return fetchRes

        where
            waitForOneMin = let micro = (6 :: Int) in timeout (10^micro * 60 * 10)
            getLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
            services = [Video, Serie, Meteo]

            fetcher queue = forever $ do
                    config   <- loadConfigFile
                    let fetchers = (\service -> (service, fetchServiceData service (getConfig config $ show service)))
                                   <$> services

                    putStrLn "---------------------------------------------------"
                    putStrLn . ("Start fetching :: " ++ ) . show =<< getLocalTime

                    handles      <- forM fetchers $ \(service, fetcher') -> do
                                       handle <- async $ waitForOneMin fetcher'
                                       return (service, handle)

                    servicesData <- forM handles $ \(service, handle) -> do
                                        res <- wait handle
                                        return $ case res of
                                                    Just d -> (service, d)
                                                    _      -> (service, None)
                    _       <- swapMVar queue servicesData

                    putStrLn . ("Done fetching :: " ++ ) . show =<< getLocalTime
                    putStrLn "---------------------------------------------------"

                    delay (1000000 * 60 * 60 * 2)


runRestServer ::  MVar [(Service, ServiceData)] -> IO ()
runRestServer queue = scotty 8086 $ do
    get "/:type/:val" $ do
        service   <- param "type" :: ActionM String
        action    <- param "val"  :: ActionM String
        services  <- liftIO $ readMVar queue

        let requestResult = dispatch action =<< lookup (strToService service) services

        case requestResult of
            Just result -> do
                           setHeader "Content-type" "application/json; charset=utf-8"
                           result

            Nothing     -> next

    notFound $ do
        status ok200
        setHeader "Content-type" "text/html; charset=utf-8"
        file "resources/index.html"


main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

