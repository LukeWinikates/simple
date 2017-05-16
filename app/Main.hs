{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

import Control.Exception (throwIO)
import Network.HTTP.Req
import Data.Aeson
import Text.Read (readMaybe)
import Lib
import System.Process (callCommand)
import Text.PrettyPrint.Boxes as B
import System.Random

charToNumber :: String -> Maybe Int
charToNumber c = (readMaybe c :: Maybe Int)

nth :: Int -> [a] -> a
nth n list = head $ drop (n-1) list

tabularize :: [GiphyItem] -> Box
tabularize items =
  (B.<+>)
    (foldl
      (B.//)
      (B.text "#")
      (map (B.text . show) [1..(length items)]))

    (foldl
      (B.//)
      (B.text "Giphy Slug")
      (map (B.text . slug) items))

main :: IO ()
main =
   getStdGen >>= \gen ->
   putStrLn "enter a search term and press <enter>" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   printBox (tabularize giphies) >>
   putStrLn "which one would you like to open? (type a number and press <enter>, or just press enter to get a random gif)" >>
   getLine >>= \l -> return (charToNumber l) >>= (\maybeNumber ->
    let (randomNumber, _) = (randomR (1, length giphies) gen) in
      case maybeNumber of
          Just number -> return number
          Nothing -> return randomNumber) >>= \n ->
   callCommand $ "open " ++ embedUrl (nth n giphies)
