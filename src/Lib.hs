
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( giphySearch, GiphyList(..), GiphyItem(..) -- had to export a ton of these so that destructuring with the GiphyList data constructor could work
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
giphySearch :: String -> IO GiphyList
giphySearch searchterms =
  let url = (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
      options = ("q" =: (searchterms :: String) <>
                          "api_key" =: ("dc6zaTOxFJmzC"::String)) in
        req GET url NoReqBody jsonResponse options >>= \res ->
        return (responseBody res :: GiphyList)



