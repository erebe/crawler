{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude

import qualified Config
import qualified RestAPI
import           Service

import           Control.Concurrent.Async        (async)
import           Control.Concurrent.Thread.Delay (delay)
import           Data.Maybe                      (fromJust)


main :: IO ()
main = do
    config <- Config.load
    guard (isJust config)

    let listenOn = Config.listenOn . Config.app $ fromJust config
    spawnServiceDaemon >>= flip RestAPI.runServer listenOn

spawnServiceDaemon :: IO (MVar [ServiceDTO])
spawnServiceDaemon = do
    channel <- newMVar []
    _ <- async $ runDaemon channel
    return channel

  where
    rescheduleIn duration = delay (1000000 * 60 * duration)
    runDaemon channel = forever $ do
        cfg <- Config.load
        guard (isJust cfg)

        services <- updateServices (Config.subscriptions (fromJust cfg))
        _ <- swapMVar channel services
        rescheduleIn . toInteger . Config.updateFrequencyInMin . Config.app $ fromJust cfg
