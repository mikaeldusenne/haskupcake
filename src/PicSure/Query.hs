{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, LambdaCase #-}
module PicSure.Query where

import Control.Concurrent (threadDelay)

import Data.Aeson
import qualified Data.HashMap.Strict as M
import System.IO
import Network.HTTP.Client


import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS

import Data.Scientific
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec (parse)

import Control.Monad.Fix

import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Utils.Misc
import PicSure.Utils.Json
import qualified PicSure.Utils.Json as J (lookup)
import PicSure.Requester
import PicSure.Resource
import PicSure.Types
import PicSure.Utils.Log

urlQueryService = "queryService"
urlRunQuery     = urlQueryService </> "runQuery"

urlResultService    = "resultService"
urlResultStatus     = urlResultService </> "resultStatus"
urlAvailableFormats = urlResultService </> "availableFormats"
urlResultDownload   = urlResultService </> "result"

oneSecond = 1000000 -- microseconds, for threaddelay

query :: [Variable] -> [Where] -> PicSureM Int
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=whereClause}
      extract = rightToMaybe . floatingOrInteger . unNumber . J.lookup "resultId"
  postRequest urlRunQuery body >>= return . fromJust . (>>=extract) . decodeValue

resultStatus :: Show a => a -> PicSureM Status
resultStatus n = getRequest (urlResultStatus</>show n) [] >>= return . fromJust . (read . unString . J.lookup "status"<$>) . decodeValue

-- resultAvailableFormats :: Int -> PicSureM Value
resultAvailableFormats n = getRequest (urlAvailableFormats</>show n) [] >>= return . fromJust . decode

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

toAlias = filter (`elem` l) . replaceStr " " "_" . basename
  where l = "-_" ++ alphaNum

-- for an alias and a pui, create the corresponding Query value
-- in case of a categorical variable, each modality will be listed
-- in the select part, but keeping the same alias so all results
-- end up in the same column.
mkQuery :: (String, String) -> PicSureM Query
mkQuery (alias, pui) = let
  f l = let
    fields = map (`Field`"String") l
    vars = map (\f -> Variable{field=f, alias=alias}) fields
    whereClauses = Where{
      field=Field{pui=pui, dataType="STRING"},
      predicate=CONTAINS,
      logicalOperator=OR,
      fields = M.fromList[("ENOUNTER","NO")]}
    in Query{select=vars, whereClauses=[whereClauses]}
  in printLog DEBUG ("mkQuery", (alias, pui)) >> lsPath' pui >>= return . f . (perhaps' isEmpty (const [pui]))

waitUntilAvailable n = resultStatus n >>=
  \case AVAILABLE -> return ()
        ERROR -> error $ "There was an error with result " ++ show n
        _ -> liftIO (threadDelay oneSecond) >> waitUntilAvailable n

-- simpleQuery :: [(String, String)] -> StateT PicState IO ()
simpleQuery :: [(String, String)] -> String -> PicSureM ()
simpleQuery cols file =
  -- todo this could be prettier somehow
  mapM (\(a, p) -> do p' <- searchPath' p                    -- resolve all paths first
                      case p' of Nothing -> error "error in requested variables. Please check the paths/spelling."
                                 Just p'' -> return (a, p'')) cols >>= \cols -> do
  let q (Query vs ws) = do
        n <- query vs ws
        waitUntilAvailable n
        fromRight <$> resultFetch n

  l <- (mconcat <$> traverse mkQuery cols) >>= q
  liftIO $ writeFile file $ CSV.genCsvFile l
