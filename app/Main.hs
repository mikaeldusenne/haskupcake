{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Resource
import Config

import Control.Monad.Trans.Reader

-- just some useless stuff
f l = lsPath' (head l) >>= (\l' -> (if l' == [] then return l else f $ l'))

main = do
  config <- readConfig "./config.json"
  -- pmsdn <- head <$> runReaderT listServices config
  -- runReaderT (buildPathTree pmsdn) config
  runReaderT (searchPath' "Clinical" >>= lsPath' >>= f) config >>= print
  --   >>= print
