{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Reddit where

import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL

import           Control.Applicative
import           Data.Maybe

import           GHC.Generics
import           Control.Lens

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(forM, join)


data Topic = Topic { _title :: T.Text
                   , _url :: String
                   , _commentLink :: String
                   , _thumbnail :: String
                   , _date :: Integer
                   , _numComments :: Integer

                   } deriving (Show, Generic)

data Reddit = Reddit { _name :: String
                     , _topics :: [Topic]

                     } deriving (Show, Generic)


$(makeLenses ''Topic)
$(makeLenses ''Reddit)


getSubRedditURL :: String -> String
getSubRedditURL subName =  "https://www.reddit.com/r/" ++ subName ++ ".json"

decodeAPI :: BL.ByteString -> Maybe Reddit
decodeAPI js = do
    result <- decode js
    join $ flip parseMaybe result $ \obj -> do
        topicsObj <- obj .: "data" >>= (.: "children") :: Parser [Object]
        topics' <- extractTopics topicsObj
        return $ Reddit <$> Just "kind" <*> topics'


    where
        extractTopics objs = return $ forM objs $ \obj ->
            flip parseMaybe obj $ \container -> do
            topic <- container .: "data"
            Topic <$> topic .: "title"
                  <*> topic .: "url"
                  <*> (("https://www.reddit.com" ++ ) <$> topic .: "permalink")
                  <*> topic .: "thumbnail"
                  <*> topic .: "created"
                  <*> topic .: "num_comments"

fetchSubReddit :: [String] -> IO [Reddit]
fetchSubReddit subRedditNames = do
    reddits <- getPages decodeAPI (getSubRedditURL <$> subRedditNames)
    return . catMaybes $ zipWith (\subName reddit -> (name.~ subName) <$> reddit)
                         subRedditNames (join <$> reddits)

