{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}

import qualified Eztv
import qualified Youtube
import System.Directory(getHomeDirectory)
import Control.Applicative((<$>))
import Control.Monad(join, forever)
import Data.Maybe(fromMaybe)
import Control.Concurrent.Async(async, wait)
import Control.Concurrent.MVar(MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.Thread.Delay(delay)

import Web.Scotty
import Control.Monad.IO.Class(liftIO)
import Data.List(find)
import Control.Lens

import Data.Aeson
import Data.Aeson.Encode.Pretty

import GHC.Generics

instance ToJSON Eztv.Episode
instance ToJSON Eztv.Serie
instance ToJSON Youtube.Video
instance ToJSON Youtube.Channel
instance ToJSON API

data API = Youtube [Youtube.Channel]
           | Serie [Eztv.Serie] deriving (Show, Generic)

-- data Elem = Elem { name :: String }

class ApiAction a where
    listA :: a -> Maybe (ActionM ())
    lastA :: a -> Maybe (ActionM ()) 
    findA :: String -> a ->  Maybe (ActionM ())
    dispatch :: String -> a -> Maybe (ActionM ())

instance ApiAction API where
    listA api | Serie series <- api     = Just $ raw . encodePretty $ series^..traverse.Eztv.serieName 
              | Youtube channels <- api = Just $ raw . encodePretty $ channels^..traverse.Youtube.name
              | otherwise  =  Nothing

    lastA api | Serie series <- api     = Just $ raw . encodePretty . join $ take 1 <$> series^..traverse.Eztv.episodes
              | Youtube channels <- api = Just $ raw . encodePretty . join $ take 1 <$> channels^..traverse.Youtube.videos
              | otherwise = Nothing


    findA toFind api | Serie series <- api     = Just $ raw . encodePretty $ fromMaybe (Eztv.Serie "" [])
                                                                           $ find (\serie -> serie^.Eztv.serieName == toFind) series
                     | Youtube channels <- api = Just $ raw . encodePretty $ fromMaybe (Youtube.Channel "" [])
                                                                           $ find (\channel -> channel^.Youtube.name == toFind) channels
                     | otherwise = Nothing

    dispatch arg | "list" <- arg = listA  
                 | "last" <- arg = lastA 
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
            fetcher queue = forever $ do
                    putStrLn "Start fetching !"
                    config     <- loadConfigFile
                    series'    <- async $ Eztv.fetchSeries (getFromConfig "eztv" config)
                    channels'  <- async $ Youtube.fetchChannels (getFromConfig "youtube" config)
                    channels'' <- wait channels'
                    series''   <- wait series'
                    putStrLn "Done fetching !"

                    let dat = [("serie", Serie series'' )
                              ,("youtube", Youtube channels'')
                              ]


                    _ <- swapMVar queue $ dat
                    delay (1000000 * 60 * 60 * 2)


runRestServer ::  MVar [(String, API)] -> IO ()
runRestServer queue = scotty 8080 $
    get "/:type/:val" $ do
        crawlerType <- param "type" :: ActionM String
        val <- param "val" :: ActionM String
        apis <- liftIO $ readMVar queue

        let res = case lookup crawlerType apis of
                        Just api -> dispatch val api
                        _        -> Nothing

        setHeader "Content-type" "application/json; charset=utf-8"
        -- liftIO $ print res
        case res of 
            Just action -> action
            Nothing -> next


main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

