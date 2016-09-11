{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}


module Service where

import qualified Eztv
import qualified HorribleSubs
import qualified Reddit           as R
import qualified Youtube          as Y

import           ClassyPrelude
import           Data.Proxy
import           Data.UnixTime

import           Data.Aeson       hiding (json)
import           Data.Aeson.TH
import           Data.Aeson.Types
-- import           System.Timeout (timeout)

instance ToJSON UnixTime where
    toJSON = toJSON . show . utSeconds


$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Eztv.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Eztv.Serie)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Y.Video)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''Y.Channel)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''HorribleSubs.Episode)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''HorribleSubs.Anime)

$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''R.Topic)
$(deriveToJSON defaultOptions {fieldLabelModifier = dropWhile (== '_')} ''R.Reddit)

data ServiceKind =  Youtube | Reddit | Serie | Anime deriving (Enum, Eq, Ord, Show, Read)

type family ServiceData (k :: ServiceKind)  where
  ServiceData 'Youtube = Y.Channel
  ServiceData 'Reddit = R.Reddit
  ServiceData 'Anime = HorribleSubs.Anime
  ServiceData 'Serie = Eztv.Serie


type family ServiceInput (k :: ServiceKind) where
  ServiceInput 'Youtube =  String
  ServiceInput 'Reddit = String
  ServiceInput 'Anime = String
  ServiceInput 'Serie = String


data Service (k :: ServiceKind) = ToJSON (ServiceData k) => Service [ServiceData k]


toJSON' :: Services a -> [Pair] -> Value
toJSON' SNil acc = object acc
toJSON' (SCons (Identity (Service d :: Service k)) ss) acc = toJSON' ss (acc <> [ name (Proxy :: Proxy k) .= toJSON d ])

instance ToJSON (Services a) where
  toJSON ss = toJSON' ss []


class MkService (k :: ServiceKind) where
  mkService :: [String] -> IO (Service k)
  name :: Proxy k -> Text

instance MkService 'Youtube where
  mkService str = Service <$> Y.fetch str
  name _ = "youtube"

instance MkService 'Reddit where
  mkService str = Service <$> R.fetch str
  name _ = "reddit"

instance MkService 'Anime where
  mkService str = Service <$> HorribleSubs.fetch str
  name _ = "anime"

instance MkService 'Serie where
  mkService str = Service <$> Eztv.fetch str
  name _ = "serie"


data ServicesT m (a :: [ServiceKind]) where
  SNil :: ServicesT m '[]
  SCons :: MkService s => m (Service s) -> ServicesT m xs -> ServicesT m (s ': xs)

type Services a = ServicesT Identity a


updateServices :: ServicesT IO a -> IO (Services a)
updateServices services = goAsync services >>= goWait
  where
    goAsync :: ServicesT IO a -> IO (ServicesT Async a)
    goAsync SNil = return SNil
    goAsync (SCons s xs) = async s >>= \s' -> fmap (SCons s') (goAsync xs)

    goWait :: ServicesT Async a -> IO (Services a)
    goWait SNil = return SNil
    goWait (SCons s xs) = waitAsync s >>= \s' -> fmap (SCons (Identity s')) (goWait xs)

    -- timeoutAfterMin :: forall a. Int -> IO a -> IO (Maybe a)
    -- timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)
