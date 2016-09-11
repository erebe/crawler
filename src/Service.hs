{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
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
import           System.Timeout   (timeout)

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

data ServiceKind =  Youtube | Reddit | Serie | Anime

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


data Service (k :: ServiceKind) where
  Service :: ToJSON (ServiceData k) => [ServiceData k] -> Service k

instance Monoid (Service k) where

toJSON' :: Services ss -> [Pair] -> Value
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

overServices :: (Functor m, Applicative m') => (forall c. m (Service c) -> m' (b (Service c))) -> ServicesT m a -> m' (ServicesT b a)
overServices _ SNil = pure SNil
overServices f (SCons x xs) = SCons <$> f x <*> overServices f xs

updateServices :: ServicesT IO a -> IO (Services a)
updateServices services = overServices (async . fmap (fromMaybe mempty) . timeoutAfterMin 10 ) services >>= overServices (fmap Identity . waitAsync)
  where
    timeoutAfterMin :: forall a. Int -> IO a -> IO (Maybe a)
    timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)
