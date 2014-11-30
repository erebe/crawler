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

import           Control.Monad                   (forM, forever)
import           Data.Maybe

main :: IO ()
main = do
    config <- Config.load
    let app = flip fmap config $ \cfg -> do
                let listenOn = Config.listenOn . Config.app $ cfg
                spawnFetcher >>= flip RestAPI.runServer listenOn

    fromMaybe (return ()) app


spawnFetcher :: IO (MVar [Service Any])
spawnFetcher = do
        fetchRes <- newMVar []
        _ <- async $ fetcher fetchRes
        return fetchRes

        where
            timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)
            rescheduleInOneHour   = delay (1000000 * 60 * 60)
            getLocalTime          = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

            fetcher queue = forever $ do
                    config <- Config.load
                    let services = fromMaybe [] (Config.subscriptions <$> config)

                    putStrLn "---------------------------------------------------"
                    putStrLn . ("Start fetching :: " ++ ) . show =<< getLocalTime


                    --TODO use Arrow
                    handles <- forM services (async . timeoutAfterMin 10 . fetch)
                    services' <- forM handles wait
                    let ret =  uncurry fromMaybe <$> zip services services'
                    _       <- swapMVar queue ret

                    putStrLn . ("Done fetching :: " ++ ) . show =<< getLocalTime
                    putStrLn "---------------------------------------------------"

                    rescheduleInOneHour

