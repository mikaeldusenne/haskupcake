{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, RecordWildCards, LambdaCase #-}
module PicSure.Types where
import Prelude hiding (readFile)

import Network.HTTP.Client
import Network.HTTP.Types(Header)
import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as Pretty
import GHC.Generics
import qualified Data.ByteString.Char8 as BS
import System.IO.Strict

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import qualified Data.Text as T

import PicSure.Utils.Misc
import PicSure.Utils.Json
import PicSure.Utils.Trees

type PicSureM a = StateT PicState IO a

type Cache = Tree String

data PicState = PicState {
  config :: Config,
  cache :: Cache}
  deriving (Show)

cacheRoot = Node "/" []


data Config = Config {
  domain :: String,
  auth :: Auth,
  debug :: Bool,
  sessionCookies :: Maybe CookieJar,
  manager :: Manager,
  cacheFile :: Maybe String
  }

defConfig = Config{domain="",
                  auth=Token "",
                  debug=True,
                  sessionCookies=Nothing,
                  cacheFile=Nothing}

instance Show Config where
  show (Config{domain=d, auth=auth, cacheFile=cf}) = show d ++ " - " ++ show auth ++ " - " ++ show cf

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    domain  <- o .:  "domain"
    debug   <- o .:? "debug"  .!= False
    cacheFile   <- o .:? "cache"
    auth    <- o .:?  "token" >>= \case
      Just t -> return $ Token t
      Nothing -> error "no authentication method found in config"
    let sessionCookies = Nothing
        manager = undefined -- disgustingly ugly?
    return Config{..}

data Status = AVAILABLE | RUNNING | STARTED | ERROR
  deriving (Show, Read)

data Field = Field {pui :: String,
                    dataType :: String}
  deriving (Generic, Show)


data Variable = Variable {field :: Field,
                          alias :: String}
  deriving (Generic, Show)


data Predicate = CONTAINS
  deriving (Show)

data LogicalOperator = OR | AND | NOT
  deriving (Show)

data Where = Where {field :: Field,
                    predicate :: Predicate,
                    logicalOperator :: LogicalOperator,
                    fields :: M.HashMap String String
                   }
  deriving(Generic, Show)
  

data Query = Query {select :: [Variable], whereClauses :: [Where]}

instance Monoid Query where
  mempty = Query [] []
  mappend (Query sa wa) (Query sb wb) = Query (sa++sb) (wa++wb)

instance ToJSON Predicate where
  toJSON e = String . T.pack $ show e

instance ToJSON LogicalOperator where
  toJSON e = String . T.pack $ show e

instance ToJSON Field 
instance ToJSON Variable
instance ToJSON Where 

-- necessity to do an explicit instance because "where" is a reverved keyword
instance ToJSON Query where
  toJSON (Query {select=select, whereClauses=whereClause}) =
    Object . M.fromList
    . filter ((\(Array v) -> (>0) . V.length $ v) . snd)
    $ [("select", Array $ V.fromList (map toJSON select)),
       ("where",  Array $ V.fromList (map toJSON whereClause))]


data Auth = Token {runToken :: String}
  deriving (Show)


data PostGet = Params [(BS.ByteString, BS.ByteString)] | Body RequestBody
