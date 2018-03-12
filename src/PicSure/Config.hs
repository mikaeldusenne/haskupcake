{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module PicSure.Config where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import PicSure.Utils.General

data Config = Config {
  token :: String,
  domain :: String
  }
  deriving (Show, Generic)

instance FromJSON Config


readConfig :: FilePath -> IO Config
readConfig f = fromJust . decode <$> BSL.readFile f
