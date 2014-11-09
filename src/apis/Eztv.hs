{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Eztv where

import           Http(getPages)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

import qualified Text.XML.Light as XML
import           Control.Applicative
import           Control.Monad
import           Data.Maybe

import           Data.UnixTime

import           Control.Lens
import           GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Episode = Episode { _name      :: T.Text
                        ,_magnetURI :: String
                        ,_date      :: String
                        ,_link      :: String
                        ,_fileName  :: String

                        } deriving (Show,Read, Generic)

data Serie = Serie { _serieName :: String
                    ,_episodes  :: [Episode]

                   } deriving (Show, Read, Generic)

$(makeLenses ''Episode)
$(makeLenses ''Serie)



extractData :: BL.ByteString -> [Episode]
extractData xmlStr = (\el ->  name      .~ (T.decodeUtf8 . BC.pack $ extractString (findElemByName "title" el))
                            $ link      .~ extractString (findElemByName "link" el)
                            $ date      .~ (show . utSeconds $ parseUnixTime webDateFormat (BC.pack $ extractString (findElemByName "pubDate" el)))
                            $ magnetURI .~ extractString (findElem "magnetURI" "http://xmlns.ezrss.it/0.1/" el)
                            $ fileName  .~ extractString (findElem "fileName" "http://xmlns.ezrss.it/0.1/" el)
                            $ Episode "" "" "" "" ""

                      ) <$> concat (findItems <$> parseXml xmlStr)
        where
            parseXml                   = XML.onlyElems . XML.parseXML
            findElemByName tagName     = XML.findElement (XML.QName tagName Nothing Nothing)
            findElem tagName tagUri    = XML.findElement (XML.QName tagName (Just tagUri) Nothing)
            findItems                  = XML.findElements (XML.QName "item" Nothing Nothing)
            extractString el           = fromMaybe "" $ XML.strContent <$> el

craftEzrssUrl :: String -> String
craftEzrssUrl nameS = protocol ++ baseUrl ++ buildArgs
    where
        protocol  = "http://"
        baseUrl   = "ezrss.it/search/index.php"
        args      = [ ("simple", "")
                     ,("mode", "rss")
                     ,("show_name", nameS)
                    ]
        buildArgs = '?' : ( drop 1 . join $
                            (\(x, y) -> "&" ++ x ++ "=" ++ y ) <$> args
                          )

fetchSeries :: [String] -> IO [Serie]
fetchSeries seriesNames = do
    episodes' <- getPages extractData (craftEzrssUrl <$> seriesNames)
    let series = (\(nameS, eps) -> serieName .~ nameS
                                 $ episodes .~ fromMaybe [] eps
                                 $ Serie "" []
                 )
                <$> zip seriesNames episodes'

    return series


