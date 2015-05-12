{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

--TODO refine export

module Reddit where

import           ClassyPrelude
import           Http                 (getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Control.Lens
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
getSubRedditURL subName =  "https://www.reddit.com/r/" <> subName <> ".json"

decodeAPI :: BL.ByteString -> Maybe Reddit
decodeAPI js = do
    v  <- decode js :: Maybe Value
    topics' <- v ^? key "data" . key "children"

    return $ Reddit "kind" (parseTopics topics')

    where
      parseTopics v = catMaybes $ v ^.. _Array . traverse . key "data" . to parseTopic
      parseTopic v = Topic
                     <$> v ^? key "title" . _String
                     <*> v ^? key "url" . _String
                     <*> v ^? key "permalink" . _String . to ("https://www.reddit.com" <>)
                     <*> v ^? key "thumbnail" . _String
                     <*> v ^? key "created" . _Integral
                     <*> v ^? key "num_comments" . _Integral

fetch :: [String] -> IO [Reddit]
fetch subRedditNames = do
    reddits <- getPages decodeAPI (getSubRedditURL <$> subRedditNames)
    return . catMaybes $ zipWith (\subName reddit -> (name.~ T.pack subName) <$> reddit)
                         subRedditNames (join <$> reddits)

