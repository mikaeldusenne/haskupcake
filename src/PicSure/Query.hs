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

-- join two csv by their id column, that is assumed to be the first column
-- joinCsv :: (Eq a, Data.String.IsString a) => [[a]] -> [[a]] -> [[a]]
joinCsv (ha : a) (hb : b) = let
  ids = union (map head a) (map head b)
  assemble a b = a' ++ tail b -- do not repeat id column
    where a' = case a of ("":xs) -> head b : tail a -- if a was empty then put the id in b
                         _ -> a
  findRow id csv = just_or_default (take n $ repeat "")
                   . safe_head
                   . filter ((==id) . head)
                   $ csv
    where n = length $ head csv
  in assemble ha hb : map (\id -> assemble (findRow id a)
                             ((findRow id b))) ids


buildQuery l = let
    fields = map ((\pui -> Field{pui=pui, dataType="STRING"}) . snd) l
    vars = zipWith (\ field (alias, _) -> (Variable{field=field, alias=alias})) fields l
    whereClauses = map (\field -> Where{
                           field=field,
                           predicate=CONTAINS,
                           logicalOperator=OR,
                           fields = M.fromList[("ENOUNTER","YES")]}) fields
  in Query{select=vars, whereClauses=whereClauses}  

-- simpleQuery :: [(String, String)] -> StateT PicState IO ()
simpleQuery :: [(String, String)] -> String -> MbStIO ()
simpleQuery cols file = do
  let (aliases, puis) = unzip cols
  puis' <- traverse searchPath' $ puis
  let join = joinCsv
      waitUntilAvailable n = 
        resultStatus n >>= \case AVAILABLE -> return ()
                                 ERROR -> error $ "There was an error with result " ++ show n
                                 _ -> liftIO (threadDelay 1000000) >> waitUntilAvailable n
      q (Query vs ws) = do
        n <- query vs ws
        waitUntilAvailable n
        fromRight <$> resultFetch n
      queryOne :: (String, String) -> MbStIO [[String]]
      queryOne (alias, pui) = let
        Query{select=vars, whereClauses=whereClauses} = buildQuery [(alias, pui)]
        in lsPath False pui >>= \case
        [] -> q $ Query vars whereClauses
        l  -> merge <$> (q $ buildQuery $ zip (map toAlias l) l)
          where merge :: [[String]] -> [[String]]
                merge ((h:_):xs) = (h:[alias]) : map f xs
                  where f (c:cs) = c : [concat cs]
    
  l <- (\l -> if length l > 1 then reduce join l else head l) <$> mapM queryOne (zip aliases puis')
  -- liftIO $ mapM_ (\(name, content) -> writeFile name $ CSV.genCsvFile content) $ zip (map fst cols) l
  liftIO $ writeFile file $ CSV.genCsvFile l
