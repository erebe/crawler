module Main where

import qualified Config
import qualified RestAPI
import           Service

import           Control.Concurrent.Async        (async)
import           Control.Concurrent.MVar         (MVar, newMVar, swapMVar)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad                   (forever, guard)
import           Data.Maybe


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
