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
import           System.IO          hiding (putStrLn)




main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  config <- Config.load :: IO (Maybe (Config.Config '[ 'Youtube, {-'Anime,-} 'Reddit, 'Serie]))
  guard (isJust config)

  updateServices (Config.subscriptions (fromJust config)) >>= putStrLn . T.decodeUtf8 . toStrict . encode
