{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Exception (throwIO)
import Network.HTTP.Req
import Data.Aeson


-- From the sample code for `req`:
-- "Just make your monad stack an instance of MonadHttp in your application
-- and start making requests, enjoy automatic connection sharing."


instance MonadHttp IO where
  handleHttpException = throwIO

-- I think this means we're making IO an instance of MonadHttp, and that:
-- MonadHttp requires a `handleHttpException` function, which we're binding to
-- So we're saying that the IO monad is now _also_ able to be the kind of IO monad that this http library needs
-- and any http exceptions thrown will be handled by Control.Exception.throwIO

-- TIL: Haskell has exceptions

main :: IO ()
main = do
  let payload = object
        [ "foo" .= (10 :: Int)
        , "bar" .= (20 :: Int) ] -- syntax from "Aeson" for specifying a json object

  -- so we're defining a json payload using the Haskell DSL

  r <- req POST
    (https "httpbin.org" /: "post") -- constructing a URL
    (ReqBodyJson payload) -- passing our Aeson object in as the json body
    jsonResponse --
    mempty       -- query params, headers, explicit port number, etc.

  print (responseBody r :: Value)

-- definition of Req.jsonResponse:
--jsonResponse :: Proxy (JsonResponse a)
--jsonResponse = Proxy

-- so I guess it's a function that applies the Proxy type to its argument,
-- and is constrained to accepting JsonResponses of some kind

-- `mempty` is a Monoid that in this case says "hey, we don't have any headers or anything else interesting to say about this"

-- and then finally we print the body of the request