{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import Lib
import Config
import Utils.General


main = do
  config <- readConfig "./config.json"
  pmsdn <- head <$> listServices config
  buildPathTree config pmsdn
  -- lsPath True config path
  --   >>= print
