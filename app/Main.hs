{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS

import qualified Data.HashMap.Strict as M
import qualified Data.Aeson.Encode.Pretty as Pretty

import PicSure.Resource
import PicSure.Config
import PicSure.Query
import PicSure.Utils.Paths
import PicSure.Types

run :: StateT PicState IO a -> IO (a, PicState)
run f = do
  config <- readConfig "./config.json"
  runStateT f $ genPicState config

puistr = "/PMSDN-dev/Demo/01 PMS Registry (Patient Reported Outcomes)/01 PMS Registry (Patient Reported Outcomes)/Demographics/Sex/Female/"
fieldstr = Field {pui=puistr,
                  dataType="STRING"}
           
vars = [Variable {field=fieldstr,
                  alias="first_alias"}]
       
whereclause = Where {field = fieldstr,
                     predicate = CONTAINS,
                     fields = M.fromList [("ENOUNTER", "YES")]}

main = do
  -- here we still are in the IO monad
  -- config <- readConfig "./config.json"
  -- pui <- readFile "pui"
  -- (`runReaderT` config) $ do
    -- now we're in the Reader Monad
  run $ do
    -- buildPathTree pmsdn
    -- searchPath' "" >>= lsPath' >>= print
    listResources >>= liftIO . BSL.putStrLn . Pretty.encodePretty

    -- debug queries id#


    -- n <- query vars [whereclause]

    -- -- and with liftIO we can do IO things
    -- liftIO $ do
    --   putStrLn $ "id: " ++ show n
    --   threadDelay 1000000

    -- resultStatus n
    -- resultStatus (n+1)
  
  -- sequence_ (map (run . (\n -> liftIO (print n) >> resultStatus n)) [110..125])
