{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

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
import GHC.Generics
import Data.Aeson.Encode.Pretty


data Crawlable = Crawlable {
                      _channels :: [Youtube.Channel]
                     ,_series   :: [Eztv.Serie]
                 } deriving (Show, Read, Generic)

$(makeLenses ''Crawlable)
instance ToJSON Eztv.Episode
instance ToJSON Eztv.Serie
instance ToJSON Youtube.Video
instance ToJSON Youtube.Channel


-- data Api = Api {
--         fetch :: [String] -> a
--        ,list  :: String -> b
--        ,last  :: b
-- }


loadConfigFile :: IO [(String, [String])]
loadConfigFile = do
    configFile <- join $ readFile . (++ "/.config/crawler.rc") <$> getHomeDirectory
    return $ read configFile


spawnFetcher :: IO (MVar Crawlable)
spawnFetcher = do
        fetchRes <- newMVar $ Crawlable [] []
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

                    _ <- swapMVar queue $ Crawlable channels'' series''
                    delay (1000000 * 60 * 60 * 2)


runRestServer ::  MVar Crawlable -> IO ()
runRestServer queue = scotty 8080 $
    get "/:type/:val" $ do
        crawlerType <- param "type" :: ActionM String
        val <- param "val" :: ActionM String
        crawl <- liftIO $ readMVar queue

        let res = case (crawlerType,val) of
                        ("serie", "list") -> encodePretty $  [serie^.Eztv.serieName | serie <- crawl^.series]
                        ("serie", "last") -> encodePretty $ join [take 1 $ serie^.Eztv.episodes | serie <- crawl^.series]
                        ("serie", _)      -> encodePretty $ fromMaybe (Eztv.Serie "" [])
                                                  $ find (\serie -> serie^.Eztv.serieName == val) (crawl^.series)

                        ("youtube", "list") -> encodePretty $ [channel^.Youtube.name | channel <- crawl^.channels]
                        ("youtube", "last") -> encodePretty $ join [take 1 $ channel^.Youtube.videos | channel <- crawl^.channels]
                        ("youtube", _)      -> encodePretty $ fromMaybe (Youtube.Channel "" [])
                                                    $ find (\channel -> channel^.Youtube.name == val) (crawl^.channels)

                        _        -> encodePretty $  ("tata" :: String)
        setHeader "Content-type" "application/json; charset=utf-8"
        -- liftIO $ print res
        raw res

main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

