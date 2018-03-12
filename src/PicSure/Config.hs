{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}
module PicSure.Config where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import PicSure.Utils.General

data Config = Config {
  token :: String,
  domain :: String,
  debug :: Bool
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    domain <- o .:  "domain"
    token  <- o .:  "token"
    debug  <- o .:? "debug" .!= False
    return Config{..}


readConfig :: FilePath -> IO Config
readConfig f = fromJust . decode <$> BSL.readFile f
