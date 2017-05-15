
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
    ( giphySearch
    ) where

import Control.Exception (throwIO)

import Network.HTTP.Req
import Data.Aeson
import Data.Monoid
import Control.Monad (mzero) -- what is mzero?

instance MonadHttp IO where
  handleHttpException = throwIO

-- as a bonus, maybe we can use the random stuff to select a random cat gif from the list

data GiphyItem = GiphyItem {
  embedUrl :: String
  , slug :: String
} deriving Show

instance FromJSON GiphyItem where
 parseJSON (Object v) =
    GiphyItem <$> v .: "embed_url" -- how does it know to turn "embed_url" into embedUrl?
           <*> v .: "slug"
 parseJSON _ = mzero

newtype GiphyList = GiphyList [GiphyItem] deriving Show

instance FromJSON GiphyList where
    parseJSON (Object o) = GiphyList <$> o .: "data" -- I have no idea why this works and various other things I tried don't
    parseJSON _ = mzero

-- what is <$>?

-- TODO: make this return an IO of GiphyList or something like that, not just assume to print it
-- TODO: print this out more nicely
giphySearch :: String -> IO ()
giphySearch searchterms =
  let options = ("q" =: (searchterms :: String) <>
                          "api_key" =: ("dc6zaTOxFJmzC"::String)) in
      do
        res <- req GET
          (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
          NoReqBody
          jsonResponse -- still expecting json, not parsing anything for now
          options
        print (responseBody res :: GiphyList) -- the key here was that this read `Value` before,
        -- but Value is just an instance of of `FromJSON`,
        -- and you can provide your own more specific type when accessing the responseBody



