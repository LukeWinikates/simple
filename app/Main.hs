{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Text.Read (readMaybe)
import Lib
import System.Process as P (callCommand)
import Text.PrettyPrint.Boxes as B
import System.Random
import Network.HTTP.Req (MonadHttp, handleHttpException)
import Control.Exception (throwIO)

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

pickAGiphy :: [GiphyItem] -> IO GiphyItem
pickAGiphy giphies = do
  gen <- getStdGen
  userSelectedNumber <- fmap charToNumber getLine
  let randomSelection = randomNumber gen (length giphies)
      n = userSelectedNumber `orElse` randomSelection
    in return (nth n giphies)

instance MonadHttp IO where
  handleHttpException = throwIO

main :: IO ()
main =
   putStrLn "enter a search term and press <enter>" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   printBox (tabularize giphies) >>
   putStrLn "which one would you like to open? (type a number and press <enter>, or just press enter to get a random gif)" >>
   pickAGiphy giphies >>= \giphy ->
   P.callCommand $ "open " ++ embedUrl giphy

