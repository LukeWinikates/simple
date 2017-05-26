{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

-- todo: extract it all into something in Lib, so that main can be simpler?
import Text.Read (readMaybe)
import Lib
import System.Process as P (callCommand)
import Text.PrettyPrint.Boxes as B
import System.Random
import System.Exit
import Network.HTTP.Req (MonadHttp, handleHttpException)
import Control.Exception (throwIO)

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

shellCommandToOpen :: GiphyItem -> String
shellCommandToOpen giphy = "open " ++ embedUrl giphy

keepPicking :: Context -> IO ()
keepPicking context =
   putStrLn "what would you like to do?" >>
   putStrLn " 1-25: open an random gif from this list in the browser" >>
   putStrLn " r: open an random gif from this list in the browser" >>
   putStrLn " s: start a new search" >>
   putStrLn " t: print the list of giphies again" >>
   putStrLn " q: quit" >>
   (getLine |> userInputToCommand) >>= \command ->
   perform context command >>= \context ->
   keepPicking context

instance MonadHttp IO where
  handleHttpException = throwIO

data UserCommand = Pick Int | PickRandom | PrintTable | NewSearch | Quit | Wat

data Context = Context {
  stdGen :: StdGen,
  giphies :: [GiphyItem]
} deriving Show

userInputToCommand :: String -> UserCommand
userInputToCommand s =
  case s of
    "r" -> PickRandom
    "" -> PickRandom
    "s" -> NewSearch
    "p" -> PrintTable
    "q" -> Quit
    _ -> case (readMaybe s :: Maybe Int) of
      Just n -> Pick n
      Nothing -> Wat

perform :: Context -> UserCommand -> IO Context
perform context (Pick n) =
  P.callCommand (shellCommandToOpen $ nth n (giphies context)) >>
  return context

perform context NewSearch =
  putStrLn "enter a search term and press <enter>" >>
  getLine >>= giphySearch >>= \(Lib.GiphyList newGiphies) ->
  printBox (tabularize (giphies context)) >>
  return context { giphies = newGiphies }

perform context PrintTable =
  printBox (tabularize (giphies context)) >>
  return context

perform context Wat =
  putStrLn "unrecognized input. Hit Ctrl + C to exit" >>
  return context

perform _ Quit =
  putStrLn "exiting..." >>
  exitSuccess

perform context PickRandom =
  let Context { stdGen = gen, giphies = gs} = context
      (n,g) = randomNumber gen (length gs) in
    P.callCommand (shellCommandToOpen $ nth n (giphies context)) >>
    return context { stdGen = g }

main :: IO ()
main = do
   putStrLn "enter a search term and press <enter>"
   (Lib.GiphyList g) <- getLine >>= giphySearch
   printBox (tabularize g)
   gen <- getStdGen
   keepPicking Context { giphies = g, stdGen = gen }

