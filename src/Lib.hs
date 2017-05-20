
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( giphySearch, GiphyList(..), GiphyItem(..)
    ) where

import Control.Exception (throwIO)

import Network.HTTP.Req
import Data.Aeson
import Data.Monoid
import Control.Monad (mzero)

instance MonadHttp IO where
  handleHttpException = throwIO

data GiphyItem = GiphyItem {
  embedUrl :: String
  , slug :: String
} deriving Show

instance FromJSON GiphyItem where
 parseJSON (Object v) =
    GiphyItem <$> v .: "embed_url"
           <*> v .: "slug"
 parseJSON _ = mzero

newtype GiphyList = GiphyList [GiphyItem] deriving Show

instance FromJSON GiphyList where
    parseJSON (Object o) = GiphyList <$> o .: "data"
    parseJSON _ = mzero

-- maybe this can be refactored to not depend on IO and depend on MonadHttp instead?
-- or on one of those  => declaration things
-- that seem to be involved for type signatures that relate to monads
giphySearch :: String -> IO GiphyList
giphySearch searchterms =
  let url = (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
      options = ("q" =: (searchterms :: String) <>
                          "api_key" =: ("dc6zaTOxFJmzC"::String)) in
        req GET url NoReqBody jsonResponse options >>= \res ->
        return (responseBody res :: GiphyList)



