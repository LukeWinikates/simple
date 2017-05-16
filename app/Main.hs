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

charToNumber :: String -> Int
charToNumber c = read c :: Int

nth :: Int -> [a] -> a
nth n list = head $ drop (n-1) list

main :: IO ()
main =
   putStrLn "enter a search term and hit enter" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   print giphies >>
   getLine >>= \l -> return (charToNumber l) >>= \number ->
   callCommand $ "open " ++ (embedUrl (nth number giphies))