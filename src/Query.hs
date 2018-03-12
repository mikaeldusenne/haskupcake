{-# LANGUAGE OverloadedStrings #-}
module Query where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as Pretty

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Text as T

import Data.Scientific

import Utils.Paths
import Utils.List
import Utils.General
import Utils.Json
import Requester
import Config

urlQueryService = "queryService"
urlRunQuery     = urlQueryService </> "runQuery"

urlResultService    = "resultService"
urlResultStatus     = urlResultService </> "resultStatus"
urlAvailableFormats = urlResultService </> "availableFormats"
urlResultDownload   = urlResultService </> "result"

data Variable = Variable {puistr :: String,
                          alias :: String,
                          datatype :: String}

instance ToJSON Variable where
  toJSON (Variable {puistr=pui, alias=alias, datatype=dt}) =
    Object $ M.fromList [("field", field), ("alias", stringToValue alias)]
    where field = Object $ M.fromList [("pui", stringToValue pui), ("datatype", stringToValue dt)]

data Where = Where

data Query = Query {select :: [Variable], whereClauses :: [Where]}

instance ToJSON Query where
  toJSON (Query {select=select, whereClauses=whereClause}) =
    Object . M.fromList
    . filter ((\(Array v) -> (>0) . V.length $ v) . snd)
    $ [("select", Array $ V.fromList (map toJSON select))]

query :: Integral n => [Variable] -> [Where] -> ReaderT Config IO n
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=[]}
      extract o = Utils.Json.lookup "resultId" o
  -- liftIO $ BSL.putStrLn body
  fromRight . floatingOrInteger . unNumber . fromJust . (extract<$>) <$> picsurePostRequest urlRunQuery body

-- resultStatus :: Integral n => [Variable] -> [Where] -> ReaderT Config IO n
resultStatus n = Pretty.encodePretty <$> picsureGetRequest (urlResultStatus</>show n) []

resultDownload n format = picsureGetRequest (urlResultDownload</>show n</>format) []

--   "where": [
--       {
--         "field": {
--           "pui": "/nhanes/Demo/demographics/demographics/AGE/",
--           "dataType": "STRING"
--         },
--         "predicate": "CONTAINS",
--         "fields": {
--           "ENOUNTER": "YES"
--         }
--       }
--   ]
-- }'
