{-# LANGUAGE OverloadedStrings    #-}

import Test.Hspec
import Data.Aeson
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "decode :: GiphyItem" $ do
    it "can decode giphy JSON into a GiphyItem" $ do
      decode "{\"slug\": \"duck-wearing-shoes\", \"embed_url\": \"https://123.org\"}" `shouldBe`
        Just (GiphyItem {slug = "duck-wearing-shoes", embedUrl = "https://123.org"})
