{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

-- todo: extract it all into something in Lib, so that main can be simpler?
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

randomNumber :: StdGen -> Int -> (Int, StdGen)
randomNumber gen len =
    randomR (1, len) gen

orElse :: Maybe a -> a -> a
orElse (Just value) _ = value
orElse Nothing altValue = altValue

-- todo: the flip on the tuple construction is maybe too clever
-- the let line is maybe too long
-- I wonder if pickAGiphy could be a little more linear, e.g. with the Maybe function
pickAGiphy :: [GiphyItem] -> StdGen -> IO (GiphyItem, StdGen)
pickAGiphy giphies gen = do
  userSelectedNumber <- fmap charToNumber getLine
  let (n, g) = fmap (flip (,) gen) userSelectedNumber `orElse` randomNumber gen (length giphies)
    in return (nth n giphies, g)

keepPicking :: [GiphyItem] -> StdGen -> IO ()
keepPicking giphies gen =
   printBox (tabularize giphies) >>
   putStrLn "which one would you like to open? (type a number and press <enter>, or just press enter to get a random gif)" >>
   pickAGiphy giphies gen >>= \(giphy, newGen) ->
   P.callCommand ("open " ++ embedUrl giphy) >>
   keepPicking giphies newGen

instance MonadHttp IO where
  handleHttpException = throwIO

-- todo: only print the table once (or offer an option to print it again)
main :: IO ()
main =
   putStrLn "enter a search term and press <enter>" >>
   getLine >>= giphySearch >>= \(Lib.GiphyList giphies) ->
   keepPicking giphies =<< getStdGen

