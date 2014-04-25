{-# LANGUAGE FlexibleContexts #-}

module Youtube where

import           Text.Regex.PCRE

import           Control.Concurrent.Async
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Control.Applicative
import qualified Control.Exception          as Ex
import           Control.Monad
import           Data.Maybe
import           System.Environment         (getArgs)

rpattern ::  BL.ByteString
rpattern =  BLC.pack "data-video-ids=\\\"[^\"]+"

rInpattern ::  BL.ByteString
rInpattern = BLC.pack "\"(.*)" 


data Video = Video { titre       :: String
                     , url       :: String
                     , thumbnail :: String
                   } deriving (Show, Read)

data Channel = Channel
             { name   :: String
             , videos :: [Video]
             } deriving (Show, Read)

getVideosChannel :: String -> String
getVideosChannel str = "http://www.youtube.com/user/" ++ str ++ "/videos"



toVideo :: String -> String -> Video
toVideo a b =
            if length a <= 15 then
                Video { titre = b,
                        url = "http://www.youtube.com/watch?v=" ++ a,
                        thumbnail = "http://i1.ytimg.com/vi/" ++ a ++ "/mqdefault.jpg"
                      }
            else
                Video { titre = a,
                        url = "http://www.youtube.com/watch?v=" ++ b,
                        thumbnail = "http://i1.ytimg.com/vi/" ++ b ++ "/mqdefault.jpg"
                      }


-- Download the file passed by url
downloadPage :: String -> IO(Maybe BL.ByteString)
downloadPage url = do
    req <- parseUrl url

    -- Hang until the server reply
    let request = req {responseTimeout = Nothing}
    response <- withManager $ httpLbs request
    guard (responseStatus response == ok200)

    return . Just $ responseBody response


extractVideos :: BL.ByteString -> [Video]
extractVideos page =
    toTuple $ matches page

    where
    matches p = getAllTextMatches $ p =~ rpattern :: [BL.ByteString]

    getInside txt = last . getAllTextSubmatches $ txt =~ rInpattern :: BL.ByteString

    toTuple (a:b:xs) = toVideo  (BLC.unpack $ getInside a)
                                (BLC.unpack $ getInside  b) : toTuple xs
    toTuple _ = []


fetchChannel :: String -> IO Channel
fetchChannel name = do
    videos <- getVideos name `Ex.catch` invalidChannelName
    return Channel { name = name, videos = videos }

    where
    invalidChannelName :: HttpException -> IO [Video]
    invalidChannelName _ = return []

    getVideos channel = (extractVideos . fromJust) <$> (downloadPage . getVideosChannel $ channel)

main :: IO()
main = do
    channelsName <- getArgs
    fetcherJobs <- mapM (async . fetchChannel) channelsName
    mapM_ ((=<<) print . wait) fetcherJobs

