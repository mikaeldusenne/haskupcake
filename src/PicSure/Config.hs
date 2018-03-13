{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards, LambdaCase #-}
module PicSure.Config where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import PicSure.Utils.Misc

data Auth = ApiKey String | Token String
  deriving (Show)

data Config = Config {
  domain :: String,
  auth :: Auth,
  debug :: Bool
  }
  deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    domain  <- o .:  "domain"
    debug   <- o .:? "debug"  .!= False
    auth    <- o .:?  "token" >>= \case
      Just t -> return $ Token t
      Nothing -> o .:?  "apiKey" >>= \case
        Just k -> return $ ApiKey k
        Nothing -> error "no authentication method found in config"
    return Config{..}


readConfig :: FilePath -> IO Config
readConfig f = fromJust . decode <$> BSL.readFile f
