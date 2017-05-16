{-# LANGUAGE OverloadedStrings    #-}
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

-- TODO: print giphies as a table

tabularize :: [GiphyItem] -> String
tabularize items =
  foldl

main :: IO ()
main =
   putStrLn "enter a search term and hit enter" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   print giphies >>
   getLine >>= \l -> (\ number ->
    callCommand $ "open " ++ embedUrl (nth number giphies))
   (charToNumber l)