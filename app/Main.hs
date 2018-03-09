{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import Lib
import Config
import Utils.General


config = BS.readFile "./token" >>= (\tok -> return (Config {domain = "https://pmsdn-dev.hms.harvard.edu", token = tok}))

-- "PMSDN-dev/Demo/01%20PMS%20Registry%20%28Patient%20Reported%20Outcomes%29/01%20PMS%20Registry%20%28Patient%20Reported%20Outcomes%29/Clinical/Cardiovascular/Has%20the%20patient%20ever%20had%20any%20of%20the%20following%20cardiovascular%20conditions%3F/Heart%20Rate%20Too%20Fast%252FSlow"

path = "PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)/01 PMS Registry (Patient Reported Outcomes)/Clinical/Cardiovascular/Has the patient ever had any of the following cardiovascular conditions?/Heart Rate Too Fast%2FSlow/"

main = do
  config <- config
  pmsdn <- head <$> listServices config
  (show <$> buildPathTree config pmsdn) >>> putStrLn >>= writeFile "tree"
  -- lsPath True config path
  --   >>= print
