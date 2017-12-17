{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Config where

import           ClassyPrelude
import           Service                   as S


import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Aeson
import           Data.Aeson.Types

import           Data.Text.IO              (readFile)
import           System.Directory          (doesFileExist, getHomeDirectory)

import           Data.Proxy
import           Text.Toml
import           Text.Toml.Types


class ParseConfig (ss :: [ServiceKind]) where
  parseCfg :: Proxy ss -> (Text -> Parser [String]) -> Parser (ServicesT IO ss)

instance ParseConfig '[] where
  parseCfg _ _ = return SNil

instance (MkService k, ParseConfig xs) => ParseConfig (k ': xs) where
  parseCfg (Proxy :: Proxy (k ': xs)) f = do
    inputs <- f $ name (Proxy :: Proxy k)
    let serviceRunner = mkService inputs :: IO (Service k)
    ret <- parseCfg (Proxy :: Proxy xs) f
    return $ SCons serviceRunner ret


instance ParseConfig a => FromJSON (Config (a :: [ServiceKind])) where
    parseJSON (Object v) = do
        servicesObj <- v .: "services"
        let helper str = servicesObj .:? str .!= []
        servicesRunners <- parseCfg (Proxy :: Proxy a) helper

        return $ MkConfig servicesRunners

    parseJSON _ = mzero



data Application = Application {
      listenOn             :: Int
    , updateFrequencyInMin :: Int
    , homepagePath         :: String
    } deriving (Show, Read)

data Config (a :: [ServiceKind]) = MkConfig {
     subscriptions :: ServicesT IO a
    }

load :: ParseConfig a => IO (Maybe (Config a))
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
        extractConfig :: ParseConfig a => MaybeT IO (Config a)
        extractConfig = do
          let configPath = "./config/crawler.cfg"
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
