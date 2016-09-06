{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Service
       -- ( mkYoutube
       --         , mkReddit
       --         , mkSerie
       --         , mkAnime
       --         , mkForecast
       --         , Service, lastUpdate, inputs, outputs
       --         , Youtube, Reddit, Serie, Anime, Forecast
       --         )
       where

import qualified Eztv
import qualified HorribleSubs
-- import qualified OpenWeather
import qualified Reddit                   as R
-- import qualified ShowRss
import qualified Youtube                  as Y

import           ClassyPrelude
import           Data.HList
import           Data.Time
import           Data.UnixTime
import           System.Timeout

import           Control.Concurrent.Async (Async, async, wait)


type ServicesFetchers services = HList (SList IO services)
type Services services = HList (SList Maybe services)
data ServiceKind =  Youtube | Reddit | Serie | Anime | Forecast deriving (Enum, Eq, Ord, Show, Read)
data ServiceT (serviceType :: ServiceKind) input output =
    MkService {
               lastUpdate :: UnixTime
              ,inputs     :: [input]
              ,outputs    :: [output]
              } deriving (Show)

type Service (k :: ServiceKind) = ServiceT k String (Ret k)
class Fetchable (k :: ServiceKind) where
    type Ret k
    name :: Proxy k -> Text
    fetcher :: Proxy k -> [String] -> IO [Ret k]

    fetch :: [String] -> IO (Service k)
    fetch ins = MkService
                <$> getUnixTime
                <*> return ins
                <*> fetcher (Proxy :: Proxy k) ins

instance Fetchable 'Youtube  where
    type Ret 'Youtube = Y.Channel
    name _ = "youtube"
    fetcher _ = Y.fetch

instance Fetchable 'Reddit  where
    type Ret 'Reddit = R.Reddit
    name _ = "reddit"
    fetcher _ = R.fetch

instance Fetchable 'Serie  where
    type Ret 'Serie = Eztv.Serie
    name _ = "serie"
    fetcher _ = Eztv.fetch

instance Fetchable 'Anime  where
    type Ret 'Anime = HorribleSubs.Anime
    name _ = "anime"
    fetcher _ = HorribleSubs.fetch


type family SList m (ss :: [ServiceKind]) :: [*] where
    SList m '[] = '[]
    SList m (x ': xs) = m (Service x) ': SList m xs

type family SListA m (ss :: [ServiceKind]) :: [*] where
    SListA m '[] = '[]
    SListA m (x ': xs) = m (IO (Service x)) ': SListA m xs

class Applicative m => SBuilder m ss where
    buildFrom :: Proxy ss -> (Text -> m [String]) -> HList (SListA m ss)

instance Applicative m => SBuilder m ('[]) where
    buildFrom _ _ = HNil


instance (Fetchable x, SBuilder m xs) => SBuilder m (x ': xs) where
    buildFrom _ getInput = fetch <$> getInput (name (Proxy :: Proxy x))
                        `HCons` buildFrom (Proxy :: Proxy xs) getInput

timeoutAfterMin :: forall a. Int -> IO a -> IO (Maybe a)
timeoutAfterMin nbMin = let micro = (6 :: Int) in timeout (10^micro * 60 * nbMin)

data HAsync = HAsync
instance (io ~ IO (Async (Maybe a)), x ~ IO a) => ApplyAB HAsync x io where
  applyAB _ = async . timeoutAfterMin 10

data Hwait = Hwait
instance (io ~ IO (Maybe a), x ~ Async (Maybe a)) => ApplyAB Hwait x io where
  applyAB _ = wait

updateServices :: ServicesFetchers ['Youtube, 'Reddit, 'Serie, 'Anime]
               -> IO (Services ['Youtube, 'Reddit, 'Serie, 'Anime])
updateServices services = do
  putStrLn "---------------------------------------------------"
  putStrLn . ("Start fetching :: " ++ ) . tshow =<< getLocalTime


  handles <- hSequence $ applyAB (HMapL HAsync) services
  services' <- hSequence $ applyAB (HMapL Hwait) handles

  putStrLn . ("Done fetching :: " ++ ) . tshow =<< getLocalTime
  putStrLn "---------------------------------------------------"

  return services'

  where
    getLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
