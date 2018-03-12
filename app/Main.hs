{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import Resource
import Config
import Query
import Utils.Paths

-- just some useless stuff
f l = lsPath' (head l) >>= (\l' -> (if l' == [] then return l else f $ l'))

run f = do
  config <- readConfig "./config.json"
  runReaderT f config >>= BSL.putStrLn
  

main = do
  config <- readConfig "./config.json"
  -- pmsdn <- head <$> runReaderT listServices config
  -- runReaderT (buildPathTree pmsdn) config
  -- runReaderT (searchPath' "Clinical" >>= lsPath' >>= f) config >>= print
  let puis = "/PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)/01 PMS Registry (Patient Reported Outcomes)"
             </> "/Clinical/Allergy/Has the patient been diagnosed with any of the following allergy-related problems?/Asthma/"

  n <- runReaderT (query [Variable {puistr=puis, datatype="STRING", alias="asthma"}] []) config
  putStrLn $ "id: " ++ show n
  threadDelay 1000000
  run $ resultStatus n
  run $ resultStatus (n+1)
  
  -- sequence_ (map (run . (\n -> liftIO (print n) >> resultStatus n)) [110..125])
