{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

--TODO refine export

module Reddit where

import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL


import           Data.Maybe

import           Control.Lens

import qualified Data.Text            as T

import           Control.Monad        (join)
import           Data.Aeson
import           Data.Aeson.Lens


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
    v  <- decode js
    let topics' = v ^. key "data" . key "children"

    return $ Reddit "kind" (parseTopics topics')

    where
      parseTopics v = catMaybes $ v ^.. traverseArray . key "data" . to parseTopic
      parseTopic v = Topic
                     <$> v ^. key "title"
                     <*> v ^. key "url"
                     <*> fmap (T.append "https://www.reddit.com") (v ^. key "permalink")
                     <*> v ^. key "thumbnail"
                     <*> v ^. key "created"
                     <*> v ^. key "num_comments"

fetch :: [String] -> IO [Reddit]
fetch subRedditNames = do
    reddits <- getPages decodeAPI (getSubRedditURL <$> subRedditNames)
    return . catMaybes $ zipWith (\subName reddit -> (name.~ T.pack subName) <$> reddit)
                         subRedditNames (join <$> reddits)

