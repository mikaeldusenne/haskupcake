{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.ByteString.Char8 as BS

import Lib
import Config


main :: IO ()
main = do
  tok <- BS.readFile "token"
  hspec $ do
    describe "Lib.listServices" $ do
      it "lists the available services for a pic-sure thing" $ do
        listServices (Config {domain = "https://pmsdn-dev.hms.harvard.edu", token = tok}) >>= (`shouldBe` ["PMSDN-dev"])
  
