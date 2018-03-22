module PicSure.Cache where

import PicSure.Utils.Trees
import PicSure.Utils.Paths
import PicSure.Types

import Data.Monoid


addToCache cache path = cache <> fromList (splitPath path)
