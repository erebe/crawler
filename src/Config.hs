{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Config where

import Subscription

import           Data.Maybe
import           Control.Monad             (guard)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           System.Directory          (doesFileExist, getHomeDirectory)
import           Text.Read                 (readMaybe)
import           Control.Applicative       ((<$>))

data Config = Config  {
        application  :: [(String, String)]
       ,subscription :: [(String, [String])]
} deriving (Show, Read)

                                          

parseProviders :: Config -> [Provider]
parseProviders cfg = catMaybes $ flip fmap (subscription cfg) $ \(proName, inn) ->
                        flip fmap (mkProvider proName) $ \provider ->
                            onSub provider (updateInputs inn)
    where
        updateInputs inn sub = sub { inputs = flip convert sub <$> inn }

loadConfigFile :: IO Config
loadConfigFile = do
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return config

        Nothing     -> do
                       putStrLn "############################################################"
                       putStrLn "#############       Config file not found     ##############"
                       putStrLn "######### Please add one at ~/.config/crawler.rc  ##########"
                       putStrLn "############################################################"
                       return $ Config [] []

    where
        extractConfig :: MaybeT IO Config
        extractConfig = do
            configPath      <- liftIO $ (++ "/.config/crawler.rc") <$> getHomeDirectory
            isConfigPresent <- liftIO $ doesFileExist configPath
            guard isConfigPresent

            cfg <- liftIO $ readMaybe <$> readFile configPath :: MaybeT IO (Maybe Config)
            guard (isJust cfg)
            return $ fromJust cfg
