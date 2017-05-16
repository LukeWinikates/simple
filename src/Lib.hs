
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

-- TODO: print this out more nicely
-- TODO: print a table of the results
giphySearch :: String -> IO GiphyList
giphySearch searchterms =
  let options = ("q" =: (searchterms :: String) <>
                          "api_key" =: ("dc6zaTOxFJmzC"::String)) in
      req GET
        (https "api.giphy.com" /: "v1" /:"gifs" /: "search")
        NoReqBody
        jsonResponse
        options >>=
        \res -> return (responseBody res :: GiphyList)
        -- I feel like this is really backwards from how I'd write this in other languages. In pseudojava, I'd do:
        -- req(GET, etc...).map(res => res.responseBody<GiphyList>()).
        -- but it seems like applying a function to a monad in haskell expects
        -- the mapped function itself to return a monad, so that you have to
        -- `return` in order to... monadetize the function call



