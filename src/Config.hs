{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           ClassyPrelude
import           Service                   as S


import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Aeson
import           Data.Aeson.Types

import           Data.Text.IO              (readFile)
import           System.Directory          (doesFileExist, getHomeDirectory)

import           Text.Toml
import           Text.Toml.Types
import Data.HList

instance FromJSON Config where
    parseJSON (Object v) = do
        appObj <- v .: "application"
        app' <- Application
                <$> appObj .:? "listenOn" .!= 8080
                <*> appObj .:? "updateFrequencyInMin" .!= 30
                <*> appObj .:? "homepagePath" .!= "thirdparty/homepage/"
        servicesObj <- v .: "services"

        let helper str = servicesObj .:? str .!= [] 
        servicesFetchers <- hSequence $ buildFrom (Proxy :: Proxy ['Youtube, 'Reddit]) helper

        return $ MkConfig app' servicesFetchers

    parseJSON _ = mzero



data Application = Application {
      listenOn             :: Int
    , updateFrequencyInMin :: Int
    , homepagePath         :: String
    } deriving (Show, Read)

data Config = MkConfig {
      app           :: Application
    , subscriptions :: ServicesFetchers ['Youtube, 'Reddit]
    }

load :: IO (Maybe Config)
load = do
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return . Just $ config

        Nothing     -> do
                       putStrLn "#########################################################################"
                       putStrLn "#############       Config file not found or Parse Error   ##############"
                       putStrLn "#############     Please add one at ~/.config/crawler.rc   ##############"
                       putStrLn "#############       Look at the example in github repo     ##############"
                       putStrLn "#########################################################################"
                       return Nothing

    where
        extractConfig :: MaybeT IO Config
        extractConfig = do
            configPath      <- liftIO $ (<> "/.config/crawler.rc") <$> getHomeDirectory
            isConfigPresent <- liftIO $ doesFileExist configPath
            guard isConfigPresent

            file <- liftIO $ Data.Text.IO.readFile configPath
            toml <- case parseTomlDoc "" file of
                         Right val -> return val
                         Left err -> do liftIO $ print err
                                        return emptyTable

            let config = parseMaybe parseJSON (toJSON toml)
            case config of
                 Just x -> return x
                 _ -> mzero
