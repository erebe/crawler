{-# LANGUAGE RecordWildCards #-}

module Config where

import           Service                   as S

import           Control.Applicative       ((<$>))
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Maybe
import           System.Directory          (doesFileExist, getHomeDirectory)
import           Text.Read                 (readMaybe)

data Application = Application {
      listenOn             :: Int
    , updateFrequencyInMin :: Int
    } deriving (Show, Read)

data Services = Services {
      youtube  :: [String]
    , reddit   :: [String]
    , serie    :: [String]
    , anime    :: [String]
    , forecast :: [String]
    } deriving (Show, Read)

data CrawlerConfig = Config {
      application :: Application
    , services    :: Services
    } deriving (Show, Read)

data AppConfig = MkConfig {
      app           :: Application
    , subscriptions :: [IO ServiceDTO]
    }


cfgToServices :: CrawlerConfig -> AppConfig
cfgToServices Config {..} = MkConfig application
                    [ YoutubeDTO <$> (mkYoutube $ Config.youtube services)
                    , RedditDTO <$> (mkReddit $ Config.reddit services)
                    , SerieDTO <$> (mkSerie $ Config.serie services)
                    , AnimeDTO <$> (mkAnime $ Config.anime services)
                    , ForecastDTO <$> (mkForecast $ Config.forecast services)
                    ]

defaultConfig :: CrawlerConfig
defaultConfig =  Config { application = Application { listenOn = 8000, updateFrequencyInMin = 60 }
                        , services    = Services { youtube  = []
                                                 , reddit   = []
                                                 , serie    = []
                                                 , anime    = []
                                                 , forecast = []
                                                 }
                        }

load :: IO (Maybe AppConfig)
load = do
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return . Just $ cfgToServices config

        Nothing     -> do
                       putStrLn "#########################################################################"
                       putStrLn "#############       Config file not found or Parse Error   ##############"
                       putStrLn "#############     Please add one at ~/.config/crawler.rc   ##############"
                       putStrLn "########################################################################"
                       putStrLn "#############              Here a default one              #############"
                       print defaultConfig
                       putStrLn "########################################################################"
                       return Nothing

    where
        extractConfig :: MaybeT IO CrawlerConfig
        extractConfig = do
            configPath      <- liftIO $ (++ "/.config/crawler.rc") <$> getHomeDirectory
            isConfigPresent <- liftIO $ doesFileExist configPath
            guard isConfigPresent

            cfg <- liftIO $ readMaybe <$> readFile configPath :: MaybeT IO (Maybe CrawlerConfig)
            guard (isJust cfg)
            return $ fromJust cfg
