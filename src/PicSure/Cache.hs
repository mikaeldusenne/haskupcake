{-# LANGUAGE DuplicateRecordFields, LambdaCase #-}
module PicSure.Cache where

import qualified Data.Binary as B

import PicSure.Utils.Trees
import PicSure.Utils.Paths
import PicSure.Utils.Misc
import PicSure.Types

import System.IO.Strict

import Control.Monad.Trans

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)

addToCache :: PicState -> [Char] -> PicState
addToCache (PicState {config=c, cache=cache}) path = PicState{
  config=c,
  cache=cache <> fromList (splitPath $ "/" </> path)
  }

cacheFetch :: String -> StateT PicState IO (Maybe (Tree String))
cacheFetch path = do
  gets cache >>= return . \case
    Empty -> Nothing
    tree -> case (treeFind tree . splitPath $ "/" </> path) of
      (Just (n@(Node _ _))) -> Just n
      _ -> Nothing

persistCache :: StateT PicState IO ()
persistCache = do
  s@PicState{config=Config{cacheFile=path}, cache=cache} <- get
  when (path/=Nothing) . liftIO $ B.encodeFile (fromJust path) cache

invalidateCache :: StateT PicState IO ()
invalidateCache = do
  config <- gets config
  put PicState{config=config, cache=Empty}
  -- persistCache
