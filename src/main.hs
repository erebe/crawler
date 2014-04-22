{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import           Control.Concurrent.Async
import qualified Data.Conduit as C
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy as BL

import qualified Text.XML.Light as XML
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import           System.Environment         (getArgs)
import Control.Monad.Trans.Maybe(MaybeT, runMaybeT)



getPage :: (C.MonadBaseControl IO m, MonadIO m) => (BL.ByteString -> IO b) -> [String] -> m [Maybe b]
getPage func urls = withManager $ \m -> mapM (runWorker m) urls
    where
        runWorker m url = do pageBody <- runMaybeT $ worker m url
                             case func <$> pageBody of
                                  Just !z -> return <$> liftIO z
                                  Nothing -> return Nothing



worker :: MonadIO m => Manager -> String -> MaybeT m BL.ByteString
worker manager url = msum $ replicate 3 fetchPage
    where
        fetchPage = do
            let request = parseUrl url
            guard(isJust request)
            let request' = (fromJust request) { responseTimeout = Nothing -- Hang until the server reply
                                               ,checkStatus = \_ _ _ -> Nothing -- Do not throw exception on exception other than 2xx
                                              }

            response <- liftIO $ httpLbs request' manager
            -- _ <- liftIO . print $ responseStatus response
            guard(statusIsSuccessful $ responseStatus response)
            return $ responseBody response


extractData :: BL.ByteString -> [(Maybe String,Maybe String,Maybe String,Maybe String,Maybe String)]
extractData xmlStr = (\el -> ( findElemByName "title" el
                              ,findElemByName "link" el
                              ,findElemByName "pubDate" el
                              ,findElem "magnetURI" "http://xmlns.ezrss.it/0.1/" el
                              ,findElem "fileName" "http://xmlns.ezrss.it/0.1/" el

                             )) <$> concat (findItems <$> parseXml xmlStr)
        where
            parseXml               = XML.onlyElems . XML.parseXML
            findElemByName name el = XML.strContent <$> XML.findElement (XML.QName name Nothing Nothing) el
            findElem name uri el   = XML.strContent <$> XML.findElement (XML.QName name (Just uri) Nothing) el
            findItems              = XML.findElements (XML.QName "item" Nothing Nothing)


craftEzrssUrl :: String -> String
craftEzrssUrl serieName = protocol ++ baseUrl ++ buildArgs
    where
        protocol  = "http://"
        baseUrl   = "ezrss.it/search/index.php"
        args      = [ ("simple", "")
                     ,("mode", "rss")
                     ,("show_name", serieName)
                    ]
        buildArgs = '?' : ( drop 1 . join $
                            (\(x, y) -> "&" ++ x ++ "=" ++ y ) <$> args
                          )

test ::  BL.ByteString -> IO ()
test str = do  let t =  extractData str
               print t
               return ()

main :: IO ()
main = do
    args <- getArgs
    m <- getPage (async . test) (craftEzrssUrl <$> args)
    _ <- mapM_ (wait . fromJust) m
    return ()







