{-# LANGUAGE DataKinds #-}

module Main where

import qualified Config
import qualified RestAPI
import           Service

import           Control.Applicative

import           Control.Concurrent.Async        (async, wait)
import           Control.Concurrent.MVar         (MVar, newMVar, swapMVar)
import           Control.Concurrent.Thread.Delay (delay)
import           Data.Time
import           System.Timeout

import           Control.Arrow                   ((&&&))
import           Control.Monad                   (forM, forever, guard)
import           Data.Maybe

main :: IO ()
main = do
    config' <- Config.load
    guard (isJust config')
    let config = fromJust config'
    
    let listenOn = Config.listenOn . Config.app $ config
    spawnCrawler >>= flip RestAPI.runServer listenOn


spawnCrawler :: IO (MVar [Service Any])
spawnCrawler = do
        resultChannel <- newMVar []
        _ <- async $ crawler resultChannel
        return resultChannel

        where
            timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)
            rescheduleIn duration = delay (1000000 * 60 * duration)
            getLocalTime          = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
            extract config = fromMaybe ([], 60)
                             ((Config.subscriptions &&& toInteger . Config.updateFrequencyInMin . Config.app)
                                <$> config
                             )

            crawler queue = forever $ do
                    (services, updateFrequency) <- extract <$> Config.load

                    putStrLn "---------------------------------------------------"
                    putStrLn . ("Start fetching :: " ++ ) . show =<< getLocalTime


                    handles <- forM services (async . timeoutAfterMin 10 . fetch)
                    services' <- forM handles wait
                    let ret =  uncurry fromMaybe <$> zip services services'
                    _       <- swapMVar queue ret

                    putStrLn . ("Done fetching :: " ++ ) . show =<< getLocalTime
                    putStrLn "---------------------------------------------------"

                    rescheduleIn updateFrequency

