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
  -- here we still are in the IO monad
  config <- readConfig "./config.json"
  pui <- readFile "pui"

  (`runReaderT` config) $ do
    -- now we're in the Reader Monad

    -- buildPathTree pmsdn
    -- searchPath' "" >>= lsPath' >>= print
    lsPath' "PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)/01 PMS Registry (Patient Reported Outcomes)/Clinical/Allergy/Has The Patient Been Diagnosed With An Allergy To A Specific Medication?/" >>= liftIO . putStrLn . unlines
    
    -- let field = (Field {pui=pui, datatype="STRING"})
    --     var = Variable {field=field, alias="alias"}
    --     whereclause = Where {field = field, predicate = CONTAINS, fields = M.fromList [("ENOUNTER", "YES")]}
  
    -- -- debug queries id#
    -- n <- query [var] [whereclause]

    -- -- and with liftIO we can do IO things
    -- liftIO $ do
    --   putStrLn $ "id: " ++ show n
    --   threadDelay 1000000

    -- resultStatus n
    -- resultStatus (n+1)
  
  -- sequence_ (map (run . (\n -> liftIO (print n) >> resultStatus n)) [110..125])
