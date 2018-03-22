module PicSure.Cache where

import PicSure.Utils.Trees
import PicSure.Utils.Paths
import PicSure.Types

import Data.Monoid


addToCache :: PicState -> [Char] -> PicState
addToCache (PicState {config=c, cache=cache}) path = PicState{
  config=c,
  cache=cache <> fromList (tail $ splitPath path)
  }
