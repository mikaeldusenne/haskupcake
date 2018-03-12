{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric #-}
module PicSure.Query where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as Pretty
import GHC.Generics

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Text as T

import Data.Scientific

import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Utils.General
import PicSure.Utils.Json
import PicSure.Requester
import PicSure.Config
import PicSure.Types

urlQueryService = "queryService"
urlRunQuery     = urlQueryService </> "runQuery"

urlResultService    = "resultService"
urlResultStatus     = urlResultService </> "resultStatus"
urlAvailableFormats = urlResultService </> "availableFormats"
urlResultDownload   = urlResultService </> "result"


query :: Integral n => [Variable] -> [Where] -> ReaderT Config IO n
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=whereClause}
      extract o = PicSure.Utils.Json.lookup "resultId" o
  -- liftIO $ BSL.putStrLn body
  fromRight . floatingOrInteger . unNumber . fromJust . (extract<$>) <$> picsurePostRequest urlRunQuery body

-- resultStatus :: Integral n => [Variable] -> [Where] -> ReaderT Config IO n
resultStatus n = Pretty.encodePretty <$> picsureGetRequest (urlResultStatus</>show n) []

resultAvailableFormats n = Pretty.encodePretty <$> picsureGetRequest (urlAvailableFormats </>show n) []

resultDownload n format = picsureGetRequest (urlResultDownload</>show n</>format) []
