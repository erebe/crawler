{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           ClassyPrelude

import qualified Config
import           Service

import           Data.Aeson
import           Data.Maybe         (fromJust)
import qualified Data.Text.Encoding as T




main :: IO ()
main = do
    config <- Config.load :: IO (Maybe (Config.Config '[ 'Youtube, 'Anime, 'Reddit, 'Serie]))
    guard (isJust config)

    updateServices (Config.subscriptions (fromJust config)) >>= putStrLn . T.decodeUtf8 . toStrict . encode
