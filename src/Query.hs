{-# LANGUAGE OverloadedStrings #-}
module Query where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Text as T

import Utils.Paths
import Utils.List
import Utils.Json
import Requester
import Config

urlQueryService = "queryService"
urlRunQuery = urlQueryService </> "runQuery"
urlResultService = "resultService"
urlResultStatus = urlQueryService </> "resultStatus"

data Variable = Variable {puistr :: String,
                          alias :: String,
                          datatype :: String
                         }

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

-- query :: p1 -> p2 -> ReaderT Config IO BSL.ByteString
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=[]}
  -- liftIO $ BSL.putStrLn body
  picsurePostRequest urlRunQuery body

-- s
-- body <- '{
--   "select": [
--       {
--         "field": {
--           "pui": "/nhanes/Demo/laboratory/laboratory/pcbs/PCB153 (ng per g)/",
--           "dataType": "STRING"
--         },
--         "alias": "pcb153"
--       },
--     {
--         "field": {
--          "pui": "/nhanes/Demo/demographics/demographics/AGE/",
--          "dataType": "STRING"
--         },
--         "alias": "Age"
--     },
-- {
--         "field": {
--           "pui": "/nhanes/Demo/demographics/demographics/SEX/female/",
--           "dataType": "STRING"
--         },
--         "alias": "Gender"
--       }, 
--   {
--             "field": {
--               "pui": "/nhanes/Demo/demographics/demographics/SEX/male/",
--               "dataType": "STRING"
--             },
--             "alias": "Gender"
--           } 
--   ],
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
