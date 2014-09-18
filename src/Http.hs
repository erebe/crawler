{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http where


-- import           Data.Maybe
import           Network.HTTP.Conduit

import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Monad.Trans.Maybe(MaybeT, runMaybeT)

import           Control.Monad
import           Control.Exception as Ex

import qualified Data.ByteString.Lazy as BL


getPages :: (BL.ByteString -> b) -> [String] -> IO [Maybe b]
getPages func urls = withManager $ \m -> mapM (runWorker m) urls
    where
        runWorker m url = do pageBody <- runMaybeT $ worker m url
                             return $ liftM func pageBody



worker :: MonadIO m => Manager -> String -> MaybeT m BL.ByteString
worker manager url = msum $ replicate 3 fetchPage
    where
        fetchPage = do
            body <- liftIO $ fetchPageImpl `Ex.catch` \(e :: HttpException) -> do
                      print e
                      return BL.empty

            guard(body /= BL.empty)
            return body

        fetchPageImpl = do
            urlRequest <- parseUrl url
            response   <- httpLbs urlRequest manager
            return $ responseBody response
