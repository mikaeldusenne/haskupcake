{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.ByteString.Char8 as BS

import Lib
import Config
import TestResults

main :: IO ()
main = do
  config <- readConfig "config.json"
  hspec $ do
    describe "Lib.listServices" $ do
      it "lists the available services for a pic-sure thing" $ do
        listServices config >>= (`shouldBe` resultListService)
  
