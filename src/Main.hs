{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           ClassyPrelude

import qualified Config
import           Service

import           Data.Maybe    (fromJust)
import           System.IO


instance Show (Services a) where
  show SNil = mempty
  show (SCons (Service d) ss) = show d <> " " <> show ss


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    config <- Config.load :: IO (Maybe (Config.Config '[ 'Youtube, 'Anime, 'Reddit]))
    guard (isJust config)
    return ()

    updateServices (Config.subscriptions (fromJust config)) >>= ClassyPrelude.print
