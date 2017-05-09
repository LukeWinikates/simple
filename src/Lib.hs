
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
    ( giphySearch
    ) where

import Control.Exception (throwIO)

import Network.HTTP.Req
import Data.Aeson
import Data.Monoid

instance MonadHttp IO where
  handleHttpException = throwIO

-- let's make a thing that searches giphy for stuff
-- giphy's json response payload will be something like "{data: [{"id": "asdfasdfa", "url": ""}]}"
-- maybe we can pipe curl a thing down and shell out to the browser to open whatever the cat gif is
-- as a bonus, maybe we can use the random stuff to select a random cat gif from the list


-- we don't need to specify a request body anymore
-- mempty needs to be replaced with something to create the query parameters

giphySearch :: IO ()
giphySearch = do
   putStrLn "enter a search term and hit enter"
   searchterms <- getLine -- I wanted to get the string as a separate commit, but couldn't get it to work
   res <- req GET
    (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
    NoReqBody
    jsonResponse -- still expecting json, not parsing anything for now
    ("q" =: (searchterms :: String) <>
          "api_key" =: ("dc6zaTOxFJmzC"::String)) -- this part was like pulling teeth
   print (responseBody res :: Value)


--this seems more or less right, but I'm kind of stuck on how to express
-- "this is going to be called only in a context where we have an IO monad,
-- but I want to define it as a pure function"



