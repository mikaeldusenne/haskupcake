{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, LambdaCase #-}
module PicSure.Query where

import Control.Concurrent (threadDelay)

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Aeson.Encode.Pretty as Pretty
import GHC.Generics
import System.IO
import Network.HTTP.Client

import System.Directory
import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import qualified Data.Text as T
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.CSV as CSV
import Text.ParserCombinators.Parsec (parse)
import Data.List

import Control.Monad.Fix

import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Utils.Misc
import PicSure.Utils.Json
import qualified PicSure.Utils.Json as J (lookup)
import PicSure.Requester
import PicSure.Resource
import PicSure.Config
import PicSure.Types


urlQueryService = "queryService"
urlRunQuery     = urlQueryService </> "runQuery"

urlResultService    = "resultService"
urlResultStatus     = urlResultService </> "resultStatus"
urlAvailableFormats = urlResultService </> "availableFormats"
urlResultDownload   = urlResultService </> "result"

oneSecond = 1000000 -- microseconds, for threaddelay

-- query :: Integral n => [Variable] -> [Where] -> StateT PicState IO n
query :: [Variable] -> [Where] -> MbStIO Int
query cols whereClause = do
  let body = encode $ Query {select=cols, whereClauses=whereClause}
      extract = rightToMaybe . floatingOrInteger . unNumber . J.lookup "resultId"
  postRequest urlRunQuery body >>= liftMaybe . (>>=extract) . decodeValue

resultStatus :: Show a => a -> MbStIO Status
resultStatus n = getRequest (urlResultStatus</>show n) [] >>= liftMaybe . (read . unString . J.lookup "status"<$>) . decodeValue

-- resultAvailableFormats :: Int -> MbStIO Value
resultAvailableFormats n = getRequest (urlAvailableFormats</>show n) [] >>= liftMaybe . decode

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

-- csvCol n csv = map (!!n) csv

-- dropAt n l = a ++ b
--   where a = take n l
--         b = drop (n+1) l

-- -- not needed
-- join two csv by their id column, that is assumed to be the first column
-- joinCsv :: (Eq a, Data.String.IsString a) => [[a]] -> [[a]] -> [[a]]
-- joinCsv (ha : a) (hb : b) = let
--   ids = union (map head a) (map head b)
--   assemble a b = a' ++ tail b -- do not repeat id column
--     where a' = case a of ("":xs) -> head b : tail a -- if a was empty then put the id in b
--                          _ -> a
--   findRow id csv = just_or_default (take n $ repeat "")
--                    . safe_head
--                    . filter ((==id) . head)
--                    $ csv
--     where n = length $ head csv
--   in assemble ha hb : map (\id -> assemble (findRow id a)
--                              ((findRow id b))) ids


-- for an alias and a pui, create the corresponding Query value
-- in case of a categorical variable, each modality will be listed
-- in the select part, but keeping the same alias so all results
-- end up in the same column.
mkQuery :: (String, String) -> MbStIO Query
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
  in lsPath' pui >>= return . f . (perhaps' isEmpty (const [pui]))

waitUntilAvailable n = resultStatus n >>=
  \case AVAILABLE -> return ()
        ERROR -> error $ "There was an error with result " ++ show n
        _ -> liftIO (threadDelay oneSecond) >> waitUntilAvailable n

-- simpleQuery :: [(String, String)] -> StateT PicState IO ()
simpleQuery :: [(String, String)] -> String -> MbStIO ()
simpleQuery cols file =
  -- todo this could be prettier somehow
  mapM (\(a, p) -> do p' <- searchPath' p                    -- resolve all paths first
                      return (a, p')) cols >>= \cols -> do
  let q (Query vs ws) = do
        n <- query vs ws
        waitUntilAvailable n
        fromRight <$> resultFetch n

  l <- (mconcat <$> traverse mkQuery cols) >>= q
  liftIO $ writeFile file $ CSV.genCsvFile l
