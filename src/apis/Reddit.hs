{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

--TODO refine export

module Reddit where

import           Http(getPages)

import qualified Data.ByteString.Lazy       as BL

import           Control.Applicative
import           Data.Maybe

import           Control.Lens

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.Types
import Control.Monad(forM, join)


data Topic = Topic { _title       :: T.Text
                   , _url         :: T.Text
                   , _commentLink :: T.Text
                   , _thumbnail   :: T.Text
                   , _date        :: Integer
                   , _numComments :: Integer
                   } deriving (Show)

data Reddit = Reddit { _name   :: T.Text
                     , _topics :: [Topic]
                     } deriving (Show)


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
                  <*> (("https://www.reddit.com" `T.append`) <$> topic .: "permalink")
                  <*> topic .: "thumbnail"
                  <*> topic .: "created"
                  <*> topic .: "num_comments"

fetch :: [String] -> IO [Reddit]
fetch subRedditNames = do
    reddits <- getPages decodeAPI (getSubRedditURL <$> subRedditNames)
    return . catMaybes $ zipWith (\subName reddit -> (name.~ T.pack subName) <$> reddit)
                         subRedditNames (join <$> reddits)

