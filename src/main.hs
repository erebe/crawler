{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP #-}

import qualified Reddit
import qualified Eztv as Serie
import qualified Youtube as Video
import qualified OpenWeather as Weather
import qualified Haruhichan as Anime

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

import           Data.Aeson hiding (json)
import           Data.Aeson.TH

import           Data.List

import           Data.Time
import           System.Timeout

import           Control.Monad.Trans.Maybe(MaybeT, runMaybeT)
import           Data.UnixTime


instance ToJSON UnixTime where
    toJSON = toJSON . show . utSeconds

#define JSON_OPTIONS defaultOptions {fieldLabelModifier = dropWhile (== '_')}
$(deriveToJSON JSON_OPTIONS ''Weather.Forecast)
$(deriveToJSON JSON_OPTIONS ''Weather.Weather)

$(deriveToJSON JSON_OPTIONS ''Serie.Episode)
$(deriveToJSON JSON_OPTIONS ''Serie.Serie)

$(deriveToJSON JSON_OPTIONS ''Video.Video)
$(deriveToJSON JSON_OPTIONS ''Video.Channel)

$(deriveToJSON JSON_OPTIONS ''Anime.Episode)
$(deriveToJSON JSON_OPTIONS ''Anime.Anime)

$(deriveToJSON JSON_OPTIONS ''Reddit.Topic)
$(deriveToJSON JSON_OPTIONS ''Reddit.Reddit)
#undef JSON_OPTIONS

newtype Configuration = Configuration [(String, [String])]

getConfig :: Configuration -> String -> [String]
getConfig (Configuration cfg) name = fromMaybe [] $ lookup name cfg


data Service = Video | Meteo | Serie | SMS | Anime |  Reddit | Unknown
               deriving (Eq)

data ServiceData = VideoData [Video.Channel]
                 | MeteoData [Weather.Weather]
                 | SerieData [Serie.Serie]
                 | AnimeData [Anime.Anime]
                 | RedditData [Reddit.Reddit]
                 | SMSData [Int]
                 | None

servicesMap :: [(Service, (String, [String] -> IO ServiceData))]
servicesMap =  [ (Video,  ("video", \args -> VideoData <$> Video.fetchChannels args))
               , (Meteo,  ("meteo", \args -> MeteoData <$> Weather.fetchWeathers args))
               , (Serie,  ("serie", \args -> SerieData <$> Serie.fetchSeries args))
               , (Anime,  ("anime", \args -> AnimeData <$> Anime.fetchAnimes args))
               , (Reddit, ("reddit",\args -> RedditData <$> Reddit.fetchSubReddit args))
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
    listA (SerieData series)   = Just . json $ series^..traverse.Serie.name
    listA (VideoData channels) = Just . json $ channels^..traverse.Video.name
    listA (MeteoData cities)   = Just . json $ Weather.city <$> cities
    listA (AnimeData animes)   = Just . json $ animes^..traverse.Anime.name
    listA (RedditData reddits) = Just . json $ reddits^..traverse.Reddit.name
    listA _                    = Nothing

    lastA (SerieData series)   = Just . json $ [serie & Serie.episodes .~ take 1 (serie^.Serie.episodes) | serie <- series]
    lastA (VideoData channels) = Just . json $ [channel & Video.videos .~ take 1 (channel^.Video.videos) | channel <- channels]
    lastA (MeteoData cities)   = Just . json $ [Weather.Weather (Weather.city city) (take 1 $ Weather.forecasts city) | city <- cities]
    lastA (AnimeData animes)   = Just . json $ [ anime & Anime.episodes .~ take 1 (anime^.Anime.episodes)| anime <- animes]
    lastA (RedditData reddits) = Just . json $ [ reddit & Reddit.topics .~ take 25 (reddit^.Reddit.topics)| reddit <- reddits]
    lastA _                    = Nothing


    findA toFind (SerieData series)   = Just . json $ series^..traversed.filtered (\serie -> toFind `isInfixOf` (serie^.Serie.name))
    findA toFind (VideoData channels) = Just . json $ channels^..traversed.filtered (\channel -> toFind `isInfixOf` (channel^.Video.name))
    findA toFind (AnimeData animes)   = Just . json $ animes^..traversed.filtered (\anime -> toFind `isInfixOf` (anime^.Anime.name))
    findA toFind (RedditData reddits) = Just . json $ reddits^..traversed.filtered (\reddit -> toFind `isInfixOf` (reddit^.Reddit.name))
    findA toFind (MeteoData cities)   = Just . json $ [ city | city <- cities, T.toLower (T.pack toFind)
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
            timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)
            rescheduleInOneHour   = delay (1000000 * 60 * 60)
            getLocalTime          = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
            services              = [Video, Serie, Meteo, Anime, Reddit]

            fetcher queue = forever $ do
                    config   <- loadConfigFile
                    let fetchers = (\service -> (service, fetchServiceData service (getConfig config $ show service)))
                                   <$> services

                    putStrLn "---------------------------------------------------"
                    putStrLn . ("Start fetching :: " ++ ) . show =<< getLocalTime

                    handles      <- forM fetchers $ \(service, fetcher') -> do
                                       handle <- async $ timeoutAfterMin 10 fetcher'
                                       return (service, handle)

                    servicesData <- forM handles $ \(service, handle) -> do
                                        res <- wait handle
                                        return $ case res of
                                                    Just d -> (service, d)
                                                    _      -> (service, None)
                    _       <- swapMVar queue servicesData

                    putStrLn . ("Done fetching :: " ++ ) . show =<< getLocalTime
                    putStrLn "---------------------------------------------------"

                    rescheduleInOneHour


runRestServer ::  MVar [(Service, ServiceData)] -> IO ()
runRestServer queue = scotty 8086 $ do
    get "/api/:type/:val" $ do
        service   <- param "type" :: ActionM String
        action    <- param "val"  :: ActionM String
        services  <- liftIO $ readMVar queue

        let requestResult = dispatch action =<< lookup (strToService service) services

        fromMaybe next requestResult


    get "/assets/:folder/:file" $ do
        folderName   <- param "folder" :: ActionM String
        fileName   <- param "file" :: ActionM String
        file ("resources/" ++ folderName ++ "/" ++ fileName)


    notFound $ do
        status ok200
        setHeader "Content-type" "text/html; charset=utf-8"
        file "resources/index.html"


main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

