{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Control.Monad.IO.Class (liftIO)

import Data.CSV
import Text.ParserCombinators.Parsec

f = do
  -- paths <- lines <$> readFile "data/paths.txt"
  Right csv <- parseFromFile csvFile "test.csv"
  let uids = tail $ map head . filter (any (=="Yes")) $ csv
      l = filter (any (`elem`uids)) $ csv
  liftIO $ do
    print l
    print $ uids
  
  -- withConfig "config.json" $ do
    -- fullpaths <- mapM searchPath' paths
    -- return fullpaths
    

main :: IO ()
main = do
  f
  putStrLn "ok"
