{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as M

import Resource
import Config
import Query
import Utils.Paths
import Types

run f = do
  config <- readConfig "./config.json"
  runReaderT f config >>= BSL.putStrLn

main = do
  config <- readConfig "./config.json"

  -- runReaderT (buildPathTree pmsdn) config
  -- runReaderT (searchPath' "Clinical" >>= lsPath' >>= f) config >>= print
  
  pui <- readFile "pui"
  let field = (Field {pui=pui, datatype="STRING"})
      var = Variable {field=field, alias="alias"}
      whereclause = Where {field = field, predicate = CONTAINS, fields = M.fromList [("ENOUNTER", "YES")]}

  -- debug queries id#
  n <- runReaderT (query [var] [whereclause]) config
  putStrLn $ "id: " ++ show n
  threadDelay 1000000
  run $ resultStatus n
  run $ resultStatus (n+1)
  
  -- sequence_ (map (run . (\n -> liftIO (print n) >> resultStatus n)) [110..125])
