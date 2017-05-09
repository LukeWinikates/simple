
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

-- giphy's json response payload will be something like "{data: [{"id": "asdfasdfa", "url": ""}]}"
-- maybe we can pipe curl a thing down and shell out to the browser to open whatever the cat gif is
-- as a bonus, maybe we can use the random stuff to select a random cat gif from the list


-- TODO: parse JSON, get the urls for each gif as a data structure (instead of printing the whole thing)
giphySearch :: String -> IO ()
giphySearch searchterms = do
   res <- req GET
    (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
    NoReqBody
    jsonResponse -- still expecting json, not parsing anything for now
    ("q" =: (searchterms :: String) <>
          "api_key" =: ("dc6zaTOxFJmzC"::String)) -- this part was like pulling teeth
   print (responseBody res :: Value)



