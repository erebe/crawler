{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DataKinds #-}

import qualified Eztv
import qualified Youtube
import System.Directory(getHomeDirectory)
import Control.Applicative((<$>))
import Control.Monad(join, forever)
import Data.Maybe(fromMaybe, catMaybes)
import Control.Concurrent.Async(async, wait)
import Control.Concurrent.MVar(MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.Thread.Delay(delay)

import Web.Scotty
import Control.Monad.IO.Class(liftIO)
import Control.Lens hiding ((.=), contains)

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Data.List(isInfixOf)
import GHC.Generics

import Data.Time
import System.Timeout

instance ToJSON Eztv.Episode
instance ToJSON Eztv.Serie
instance ToJSON Youtube.Video
instance ToJSON Youtube.Channel
instance ToJSON API

data API = Youtube [Youtube.Channel]
         | Serie [Eztv.Serie]
         deriving (Show, Generic)

buildSerie :: [String] -> IO (String, API)
buildSerie args = (\res -> ("serie", Serie res)) <$> Eztv.fetchSeries args
buildYoutube :: [String] -> IO (String, API)
buildYoutube args = (\res -> ("youtube", Youtube res)) <$> Youtube.fetchChannels args


class ApiAction a where
    listA :: a -> Maybe (ActionM ())
    lastA :: a -> Maybe (ActionM ())
    findA :: String -> a ->  Maybe (ActionM ())
    dispatch :: String -> a -> Maybe (ActionM ())

instance ApiAction API where
    listA api | Serie series <- api     = Just $ raw . encodePretty $ series^..traverse.Eztv.serieName
              | Youtube channels <- api = Just $ raw . encodePretty $ channels^..traverse.Youtube.name
              | otherwise  =  Nothing

    lastA api | Serie series <- api     = Just $ raw . encodePretty $ [serie & Eztv.episodes .~ take 1 (serie^.Eztv.episodes) | serie <- series]
              | Youtube channels <- api = Just $ raw . encodePretty $ [channel & Youtube.videos .~ take 1 (channel^.Youtube.videos) | channel <- channels]
              | otherwise = Nothing


    findA toFind api | Serie series <- api     = Just $ raw . encodePretty $ series^..traversed.filtered (\serie -> toFind `isInfixOf` (serie^.Eztv.serieName))
                     | Youtube channels <- api = Just $ raw . encodePretty $ channels^..traversed.filtered (\channel -> toFind `isInfixOf` (channel^.Youtube.name))
                     | otherwise = Nothing

    dispatch arg | "list" <- arg = listA
                 | "last" <- arg = lastA
                 | ""     <- arg = lastA
                 | otherwise = findA arg






loadConfigFile :: IO [(String, [String])]
loadConfigFile = do
    configFile <- join $ readFile . (++ "/.config/crawler.rc") <$> getHomeDirectory
    return $ read configFile


spawnFetcher :: IO (MVar [(String, API)])
spawnFetcher = do
        fetchRes <- newMVar []
        _ <- async $ fetcher fetchRes
        return fetchRes

        where
            getFromConfig key cfg = fromMaybe [] $ lookup key cfg
            waitForOneMin = let micro = (6 :: Int) in timeout (10^micro * 60)

            fetcher queue = forever $ do
                    putStrLn "Start fetching !"
                    config     <- loadConfigFile
                    let apis = [ buildSerie (getFromConfig "eztv" config)
                               , buildYoutube (getFromConfig "youtube" config)
                               ]
                    handles <- mapM (async . waitForOneMin)  apis
                    dat <- catMaybes <$> mapM wait handles

                    _ <- swapMVar queue dat
                    getCurrentTime >>= putStrLn . ("Done fetching :: " ++ ) . show
                    delay (1000000 * 60 * 60 * 2)


runRestServer ::  MVar [(String, API)] -> IO ()
runRestServer queue = scotty 8086 $
    get "/:type/:val" $ do
        crawlerType <- param "type" :: ActionM String
        val <- param "val" :: ActionM String
        apis <- liftIO $ readMVar queue

        let res = case lookup crawlerType apis of
                        Just api -> dispatch val api
                        _        -> Nothing

        setHeader "Content-type" "application/json; charset=utf-8"
        -- liftIO $ print res
        let action = fromMaybe next res
        action


main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

