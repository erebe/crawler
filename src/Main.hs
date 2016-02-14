{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude

import qualified Config
import qualified RestAPI
import           Service

import           Control.Concurrent.Async        (async)
import           Control.Concurrent.Thread.Delay (delay)
import           Data.HList
import           Data.Maybe                      (fromJust)
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    config <- Config.load
    guard (isJust config)

    let listenOn = Config.listenOn . Config.app $ fromJust config
    let homepagePath = Config.homepagePath . Config.app $ fromJust config
    spawnServiceDaemon >>= RestAPI.runServer listenOn (homepagePath <> "/")

spawnServiceDaemon :: IO (MVar (Services ['Youtube, 'Reddit, 'Serie, 'Anime]))
spawnServiceDaemon = do
    channel <- newMVar (Nothing .*. Nothing .*. Nothing .*. Nothing .*. HNil)
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
