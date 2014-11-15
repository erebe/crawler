{-# LANGUAGE DataKinds #-}

module Subscription where

import qualified Eztv        as Serie
import qualified Haruhichan  as Anime
import qualified OpenWeather as Weather
import qualified Reddit
import qualified Youtube

-- import           Control.Applicative             ((<$>))

data Subscription input output = Subscription {
                name    :: String
               ,inputs  :: [input]
               ,outputs :: [output]
               ,fetch   :: [input] -> IO [output]
            }

data Provider = Youtube (Subscription String Youtube.Channel)
              | Eztv (Subscription String Serie.Serie)
              | Anime (Subscription String Anime.Anime)
              | Reddit (Subscription String Reddit.Reddit)
              | Weather (Subscription String Weather.Weather)

mkSubscription :: String -> Maybe Provider
mkSubscription "youtube" = return $ Youtube $ Subscription "youtube" [] [] Youtube.fetchChannels
mkSubscription "serie"   = return $ Eztv $ Subscription "serie" [] [] Serie.fetchSeries 
mkSubscription "anime"   = return $ Anime $ Subscription "anime" [] [] Anime.fetchAnimes
mkSubscription "reddit"  = return $ Reddit $ Subscription "reddit" [] [] Reddit.fetchSubReddit
mkSubscription "weather" = return $ Weather $ Subscription "weather" [] [] Weather.fetchWeathers
mkSubscription _         = Nothing
--
--
-- fetch :: Subscription -> IO Subscription
-- fetch (Youtube args  _) = Youtube args <$> Youtube.fetchChannels args
-- fetch (Serie args    _) = Serie args   <$> Serie.fetchSeries args
-- fetch (Anime args    _) = Anime args   <$> Anime.fetchAnimes args
-- fetch (Weather args  _) = Weather args <$> Weather.fetchWeathers args
-- fetch (Reddit args   _) = Reddit args  <$> Reddit.fetchSubReddit args
--
--
--
--
-- data Config = Config  {
--         application :: [(String, String)]
--        ,subscription :: [(String, String)]
--     } deriving (Show, Read)
