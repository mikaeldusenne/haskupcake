{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, LambdaCase #-}
module PicSure.Query where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as Pretty
import GHC.Generics
import System.IO
import Network.HTTP.Client

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import qualified Data.Text as T
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec

import Control.Monad.Fix

import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Utils.Misc
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


-- query :: Integral n => [Variable] -> [Where] -> StateT PicState IO n
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=whereClause}
      extract o = PicSure.Utils.Json.lookup "resultId" o
  -- liftIO $ BSL.putStrLn body
  fromRight . floatingOrInteger . unNumber . fromJust . (extract<$>) . (>>=decodeValue) <$> postRequest urlRunQuery body

-- resultStatus :: Show a => a -> StateT PicState IO BSL.ByteString
resultStatus n = Pretty.encodePretty . (>>=decodeValue) <$> getRequest (urlResultStatus</>show n) []

-- resultAvailableFormats :: Show a => a -> StateT PicState IO (Maybe [String])
resultAvailableFormats n = (>>=decode) <$> getRequest (urlAvailableFormats</>show n) []

urlResultDownloadCSV n = (urlResultDownload</>show n</>"CSV")

-- returns the result as a CSV variable
resultFetch n = parse CSV.csvFile "" . BS.unpack
                <$> request' (urlResultDownloadCSV n) (Params []) ((BS.concat <$>) . brConsume . responseBody)

-- Stream, do not try to store the whole response in memory
resultDownload n file = request' (urlResultDownloadCSV n) (Params []) $ \resp ->
  withFile file AppendMode $ \h -> do -- open the file in append mode
  fix $ \loop -> do -- monad fix to loop until there is no byte left
    bytes <- brRead $ responseBody resp -- get the next bytes
    if BS.null bytes
      then putStrLn "Done." -- if it's null, we're done
      else do
      BS.hPut h bytes -- otherwise we write the bytes to the file
      loop            -- and we keep "looping"
