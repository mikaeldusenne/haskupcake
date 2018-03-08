{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import Lib
import Config
import General


config = BS.readFile "./token" >>= (\tok -> return (Config {domain = "https://pmsdn-dev.hms.harvard.edu", token = tok}))

main = do
  config <- config
  pmsdn <- head <$> listServices config
  -- (show <$> buildPathTree config pmsdn) >>> putStrLn >>= writeFile "tree"
  lsPath config "PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)"
    >>= print
