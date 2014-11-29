{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Config (load) where

import           Service              as S

import           Control.Applicative       ((<$>))
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Maybe
import           System.Directory          (doesFileExist, getHomeDirectory)
import           Text.Read                 (readMaybe)

import           Data.UnixTime

data CrawlerConfig = Config {
      youtube  :: Config S.Youtube
    , reddit   :: Config S.Reddit
    , serie    :: Config S.Serie
    , anime    :: Config S.Anime
    , forecast :: Config S.Forecast

    } deriving (Show, Read)



cfgToServices :: CrawlerConfig -> [Service Any]
cfgToServices cfg = [ fromConfig $ Config.youtube cfg
                    , fromConfig $ Config.reddit cfg
                    , fromConfig $ Config.serie cfg
                    , fromConfig $ Config.anime cfg
                    , fromConfig $ Config.forecast cfg
                    ]

class (Provide a) => FromConfig a where
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


load :: IO [Service Any]
load = do
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return $ cfgToServices config

        Nothing     -> do
                       putStrLn "############################################################"
                       putStrLn "#############       Config file not found     ##############"
                       putStrLn "######### Please add one at ~/.config/crawler.rc  ##########"
                       putStrLn "############################################################"
                       return []

    where
        extractConfig :: MaybeT IO CrawlerConfig
        extractConfig = do
            configPath      <- liftIO $ (++ "/.config/crawler.rc") <$> getHomeDirectory
            isConfigPresent <- liftIO $ doesFileExist configPath
            guard isConfigPresent

            cfg <- liftIO $ readMaybe <$> readFile configPath :: MaybeT IO (Maybe CrawlerConfig)
            guard (isJust cfg)
            return $ fromJust cfg
