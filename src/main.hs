{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async
import qualified Data.Conduit as C
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import           Control.Monad.IO.Class(MonadIO)
import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import           System.Environment         (getArgs)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.Trans.Maybe(runMaybeT)
import Control.Monad.Trans.Class(lift)



-- getPage :: (C.MonadBaseControl IO m, MonadIO m) => (BL.ByteString -> b) -> [String] -> m [Maybe b]
getPage func urls = withManager $ \m -> mapM (\url -> do
                                                      pageBody <- runMaybeT $ worker m url
                                                      toto <- liftIO $ func <$> pageBody
                                                      return "toto"
                                             ) urls



worker :: MonadIO m => Manager -> String -> MaybeT m (BL.ByteString)
worker manager url = msum $ replicate 1 fetchPage
    where
        fetchPage = do
            let request = parseUrl url
            guard(isJust request)
            let request' = (fromJust request) { responseTimeout = Nothing -- Hang until the server reply
                                               ,checkStatus = \_ _ _ -> Nothing -- Do not throw exception on exception other than 2xx
                                              }

            response <- liftIO $ httpLbs request' manager
            _ <- liftIO . print $ responseStatus response
            guard(statusIsSuccessful $ responseStatus response)
            return $ responseBody response



craftEzrssUrl :: String -> String
craftEzrssUrl serieName = protocol ++ baseUrl ++ buildArgs
    where
        protocol  = "http://"
        baseUrl   = "ezrss.it/search/index.php"
        args      = [ ("simple", "")
                     ,("mode", "rss")
                     ,("show_name", serieName)
                    ]
        buildArgs = "?" ++ ( drop 1 $
                             join $
                             (\(x, y) -> "&" ++ x ++ "=" ++ y ) <$> args
                           )

main :: IO ()
main = do
    args <- getArgs
    m <- (getPage (\x -> print x) (craftEzrssUrl <$> args))
    -- _ <- mapM_ (\w -> fromJust w ) m
    print "done"
