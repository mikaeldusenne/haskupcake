{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Config where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import Utils.General

data Config = Config {
  token :: String,
  domain :: String
  }
  deriving (Show, Generic)

instance FromJSON Config


readConfig :: FilePath -> IO Config
readConfig f = do
  s <- BSL.readFile f
  return . fromJust $ decode s
