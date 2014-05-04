{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.ByteString.Lazy.Char8 as BC


data Crawlable = Crawlable {
                      _channels :: [Youtube.Channel]
                     ,_series   :: [Eztv.Serie]
                 } deriving (Show, Read)

$(makeLenses ''Crawlable)

loadConfigFile :: IO [(String, [String])]
loadConfigFile = do
    configFile <- join $ readFile <$> (++ "/.config/crawler.rc") <$> getHomeDirectory
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
runRestServer queue = scotty 8086 $
    get "/:type/:val" $ do
        crawlerType <- param "type" :: ActionM String
        val <- param "val" :: ActionM String
        crawl <- liftIO $ readMVar queue

        let res = case crawlerType of
                        "serie"  -> show $ fromMaybe (Eztv.Serie "" [])
                                         $ find (\serie -> serie^.Eztv.serieName == val) (crawl^.series)
                        "youtube" ->show $ fromMaybe (Youtube.Channel "" [])
                                         $ find (\channel -> channel^.Youtube.name == val) (crawl^.channels)

                        _        -> "tata"


        raw $ BC.pack res

main :: IO ()
main = do
    queue <- spawnFetcher
    runRestServer queue

