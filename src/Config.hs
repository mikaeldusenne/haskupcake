{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Config where

import qualified Data.ByteString.Char8 as BS

data Config = Config {
  token :: BS.ByteString,
  domain :: String
  }

