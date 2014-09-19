{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

import           Control.Applicative             ((<$>))
import           Control.Concurrent.Async        (async, wait)
import           Control.Concurrent.MVar         (MVar, newMVar, readMVar, swapMVar)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad                   (forever, guard)
import           Data.Maybe                      (catMaybes, fromMaybe)
import qualified Eztv
import           System.Directory                (getHomeDirectory, doesFileExist)
import qualified Youtube

import           Control.Lens                    hiding ((.=))
import           Control.Monad.IO.Class          (liftIO)
import           Web.Scotty

import           Data.Aeson
import           Data.Aeson.Encode.Pretty

import           Data.List                       (isInfixOf)
import           GHC.Generics

import           Data.Time
import           System.Timeout

import           Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

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
      cfg <-  runMaybeT extractConfig
      case cfg of
        Just config -> return config

        Nothing     -> do
                       putStrLn "############################################################"
                       putStrLn "#############       Config file not found     ##############"
                       putStrLn "######### Please add one at ~/.config/crawler.rc  ##########"
                       putStrLn "############################################################"
                       return []

    where
        extractConfig :: MaybeT IO [(String, [String])]
        extractConfig = do
            configPath    <- liftIO $ (++ "/.config/crawler.rc") <$> getHomeDirectory
            configPresent <- liftIO $  doesFileExist configPath
            guard configPresent

            liftIO $ read <$> readFile configPath


spawnFetcher :: IO (MVar [(String, API)])
spawnFetcher = do
        fetchRes <- newMVar []
        _ <- async $ fetcher fetchRes
        return fetchRes

        where
            getFromConfig key cfg = fromMaybe [] $ lookup key cfg
            waitForOneMin = let micro = (6 :: Int) in timeout (10^micro * 60 * 10)

            fetcher queue = forever $ do
                    getCurrentTime >>= putStrLn . ("Start fetching :: " ++ ) . show
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
        let action = fromMaybe next res
        action


main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

