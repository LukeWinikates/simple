
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( giphySearch, GiphyList(..), GiphyItem(..)
    ) where

import Network.HTTP.Req
import Data.Aeson
import Data.Monoid
import Control.Monad (mzero)

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

(|>) :: Functor f => f a -> (a -> b) -> f b
a |> b = fmap b a

giphySearch :: MonadHttp m => String -> m GiphyList
giphySearch searchterms =
  let url = (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
      options = ("q" =: (searchterms :: String) <>
                          "api_key" =: ("dc6zaTOxFJmzC"::String)) in
      req GET url NoReqBody jsonResponse options |> responseBody
