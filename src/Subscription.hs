{-# LANGUAGE RankNTypes #-}


module Subscription 
                    -- ( Subscription, name, inputs, outputs, convert
                    -- , Provider(Youtube, Serie, Anime, Reddit, Forecast)
                    -- , onSub
                    -- , onSubM
                    -- , mkProvider
                    -- ) 
                    where

import qualified Eztv
import qualified Haruhichan
import qualified OpenWeather
import qualified Reddit
import qualified Youtube

import           Control.Applicative ((<$>))

data Service = Youtubee


data Subscription input output = Subscription {
               inputs  :: [input]
               ,outputs :: [output]
               -- ,fetcher :: [input] -> IO [output]

            } deriving (Show)

data Provider = Youtube (Subscription String Youtube.Channel)
              | Serie (Subscription String Eztv.Serie)
              | Anime (Subscription String Haruhichan.Anime)
              | Reddit (Subscription String Reddit.Reddit)
              | Forecast (Subscription String OpenWeather.Weather)

              deriving (Show)

name :: Provider -> String
name (Youtube _)  = "youtube"
name (Serie _)    = "serie"
name (Reddit _)   = "reddit"
name (Anime _)    = "anime"
name (Forecast _) = "forecast"

fetch :: Provider -> IO Provider
fetch (Youtube sub)   = Youtube  <$> ((\out -> sub { outputs = out} ) <$> Youtube.fetchChannels (inputs sub))
fetch (Serie sub)     = Serie    <$> ((\out -> sub { outputs = out} ) <$> Eztv.fetchSeries (inputs sub))
fetch (Reddit sub)    = Reddit   <$> ((\out -> sub { outputs = out} ) <$> Reddit.fetchSubReddit (inputs sub))
fetch (Anime sub)     = Anime    <$> ((\out -> sub { outputs = out} ) <$> Haruhichan.fetchAnimes (inputs sub))
fetch (Forecast sub)  = Forecast <$> ((\out -> sub { outputs = out} ) <$> OpenWeather.fetchWeathers (inputs sub))


onSub :: Provider -> (forall a b. Subscription a b -> Subscription a b) -> Provider
onSub (Youtube sub) f  = Youtube  $ f sub
onSub (Serie sub) f    = Serie    $ f sub
onSub (Reddit sub) f   = Reddit   $ f sub
onSub (Anime sub) f    = Anime    $ f sub
onSub (Forecast sub) f = Forecast $ f sub

onSubM :: Functor m => Provider -> (forall a b. Subscription a b -> m (Subscription a b)) -> m Provider
onSubM (Youtube sub) f  = Youtube  <$> f sub
onSubM (Serie sub) f    = Serie    <$> f sub
onSubM (Reddit sub) f   = Reddit   <$> f sub
onSubM (Anime sub) f    = Anime    <$> f sub
onSubM (Forecast sub) f = Forecast <$> f sub


-- fetch :: Provider -> IO Provider
-- fetch provider = onSubM provider (\sub -> (\outs -> sub { outputs = outs }) <$> fetcher sub (inputs sub))
--
-- mkProvider :: String -> Maybe Provider
-- mkProvider "youtube" = return $ Youtube (Subscription "youtube" [] [])
-- mkProvider "serie"   = return $ Serie (Subscription "serie" [] [])
-- mkProvider "anime"   = return $ Anime (Subscription "anime" [] [])
-- mkProvider "reddit"  = return $ Reddit (Subscription "reddit" [] []) 
-- mkProvider "weather" = return $ Forecast (Subscription "forecast" [] []) 
-- mkProvider _         = Nothing

