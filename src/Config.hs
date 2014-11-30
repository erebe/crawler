{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Config (load, AppConfig, app, subscriptions, listenOn) where

import           Service              as S

import           Control.Applicative       ((<$>))
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Maybe
import           System.Directory          (doesFileExist, getHomeDirectory)
import           Text.Read                 (readMaybe)

import           Data.UnixTime

data Application = Application {
     listenOn :: Int
    } deriving (Show, Read)

data Services = Services {
      youtube  :: Config S.Youtube
    , reddit   :: Config S.Reddit
    , serie    :: Config S.Serie
    , anime    :: Config S.Anime
    , forecast :: Config S.Forecast
    } deriving (Show, Read)

data CrawlerConfig = Config {
      application :: Application
    , services :: Services
    } deriving (Show, Read)

data AppConfig = MkConfig {
      app :: Application
    , subscriptions :: [Service Any]
    } deriving (Show)


cfgToServices :: CrawlerConfig -> AppConfig
cfgToServices Config {..} = MkConfig application
                    [ fromConfig . Config.youtube  $ services
                    , fromConfig . Config.reddit   $ services
                    , fromConfig . Config.serie    $ services
                    , fromConfig . Config.anime    $ services
                    , fromConfig . Config.forecast $ services
                    ]

defaultConfig :: CrawlerConfig
defaultConfig =  Config {  application = Application { listenOn = 8000 }
                         , services    = Services { youtube  = Config.Youtube []
                                                  , reddit   = Config.Reddit []
                                                  , serie    = Config.Serie []
                                                  , anime    = Config.Anime []
                                                  , forecast = Config.Forecast []
                                                  }
                        }

class FromConfig (a :: S.ServiceKind) where
    data Config a
    fromConfig :: Config a -> Service Any

instance FromConfig S.Youtube where
    data Config S.Youtube = Youtube [String] deriving (Show, Read)
    fromConfig (Config.Youtube ins) = S.YoutubeS . S.MkYoutube $ S.MkContext (UnixTime 0 0) ins []

instance FromConfig S.Serie where
    data Config S.Serie = Serie [String] deriving (Show, Read)
    fromConfig (Config.Serie ins) = S.SerieS . S.MkSerie $ S.MkContext (UnixTime 0 0) ins []

instance FromConfig S.Reddit where
    data Config S.Reddit = Reddit [String] deriving (Show, Read)
    fromConfig (Config.Reddit ins) = S.RedditS . S.MkReddit $ S.MkContext (UnixTime 0 0) ins []

instance FromConfig S.Anime where
    data Config S.Anime = Anime [String] deriving (Show, Read)
    fromConfig (Config.Anime ins) = S.AnimeS . S.MkAnime $ S.MkContext (UnixTime 0 0) ins []

instance FromConfig S.Forecast where
    data Config S.Forecast = Forecast [String] deriving (Show, Read)
    fromConfig (Config.Forecast ins) = S.ForecastS . S.MkForecast $ S.MkContext (UnixTime 0 0) ins []


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
