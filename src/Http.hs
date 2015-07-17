{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http (getPages) where


import           ClassyPrelude                hiding (replicate)
import           Data.List
import           Network.HTTP.Conduit


import           Control.Monad.Trans.Maybe    (MaybeT, runMaybeT)
import           Control.Monad.Trans.Resource

import           Control.Exception            as Ex
import           Control.Monad                (msum)

import qualified Data.ByteString.Lazy         as BL

import           System.IO                    hiding (hPutStrLn)


getPages :: (BL.ByteString -> b) -> [String] -> IO [Maybe b]
getPages func urls = withManager $ \m -> mapM (runWorker m) urls
    where
        runWorker m url = do pageBody <- runMaybeT $ worker m url
                             return $ func <$> pageBody



worker :: Manager -> String -> MaybeT (ResourceT IO) BL.ByteString
worker manager url = msum $ replicate 3 fetchPage
    where
      fetchPage = do
          body <- liftIO $ fetchPageImpl `Ex.catch` \(e :: HttpException) -> do
              hPrint stderr ((show e) <> " " <> url)
              return BL.empty

          guard(body /= BL.empty)
          return body

      fetchPageImpl = do
          urlRequest <- parseUrl url
          response   <- httpLbs urlRequest manager
          return $ responseBody response
