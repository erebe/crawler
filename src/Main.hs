{-# LANGUAGE BangPatterns      #-}
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
    args <- getArgs

    case args of
        ["--oneshot"] -> updateServices (Config.subscriptions (fromJust config)) >>= ClassyPrelude.print
        _             -> spawnServiceDaemon >>= runRestServer (fromJust config)

runRestServer :: Config.Config -> MVar (Services ['Youtube, 'Reddit, 'Serie, 'Anime]) -> IO ()
runRestServer cfg = RestAPI.runServer listenOn (homepagePath <> "/")
  where
    listenOn = Config.listenOn . Config.app $ cfg
    homepagePath = Config.homepagePath . Config.app $ cfg

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
        let !ss = services
        _ <- swapMVar channel ss
        rescheduleIn . toInteger . Config.updateFrequencyInMin . Config.app $ fromJust cfg
