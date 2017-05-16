{-# LANGUAGE OverloadedStrings    #-}
--https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ??? did not google

module Main (main) where

import Control.Exception (throwIO)
import Network.HTTP.Req
import Data.Aeson
import Lib
import System.Process (callCommand)

main :: IO ()
main =
   putStrLn "enter a search term and hit enter" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   callCommand $ "open " ++ (embedUrl (head giphies))