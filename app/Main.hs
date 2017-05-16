{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ??? did not google

module Main (main) where

import Control.Exception (throwIO)
import Network.HTTP.Req
import Data.Aeson
import Lib
import System.Process (callCommand)
import Text.PrettyPrint.Boxes as B

charToNumber :: String -> Int
charToNumber c = read c :: Int

nth :: Int -> [a] -> a
nth n list = head $ drop (n-1) list

-- TODO: add numbers
-- TODO:
tabularize :: [GiphyItem] -> Box
tabularize items =
  foldl
    (B.//)
    (B.text "SLUG")
    (map (B.text . slug) items)

main :: IO ()
main =
   putStrLn "enter a search term and press <enter>" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   printBox (tabularize giphies) >>
   putStrLn "which one would you like to open? (type a number and press <enter>)" >>
   getLine >>= \l -> (\ number ->
    callCommand $ "open " ++ embedUrl (nth number giphies))
   (charToNumber l)