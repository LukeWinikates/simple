{-# LANGUAGE OverloadedStrings    #-}

module Main (main) where

import Text.Read (readMaybe)
import Lib
import System.Process as P (callCommand)
import Text.PrettyPrint.Boxes as B
import System.Random

charToNumber :: String -> Maybe Int
charToNumber c = readMaybe c :: Maybe Int

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

randomNumber :: StdGen -> Int -> Int
randomNumber gen len =
    let (number, _) = randomR (1, len) gen in number

orElse :: Maybe a -> a -> a
orElse (Just value) _ = value
orElse Nothing altValue = altValue

main :: IO ()
main =
   getStdGen >>= \gen ->
   putStrLn "enter a search term and press <enter>" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   printBox (tabularize giphies) >>
   putStrLn "which one would you like to open? (type a number and press <enter>, or just press enter to get a random gif)" >>
   getLine >>= (\l ->
      return (orElse (charToNumber l) (randomNumber gen (length giphies)))) >>= \n ->
   P.callCommand $ "open " ++ embedUrl (nth n giphies)
