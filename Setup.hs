#! /usr/bin/env runhaskell

import Distribution.Simple
import Text.Printf(printf)
import System.Directory(copyFile)
import Data.Char

getPackgeName ::  Package pkg => pkg -> String
getPackgeName =  read . drop  11 . show . packageName

-- copyBinary args = 
main = defaultMainWithHooks simpleUserHooks { postBuild = (\a _ c _ -> do 
                                                                      let pkgName = getPackgeName c
                                                                      copyFile ("./dist/build/" ++ pkgName ++ "/" ++ pkgName) pkgName ) }
