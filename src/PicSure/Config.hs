{-# LANGUAGE LambdaCase #-}
module PicSure.Config where
import Network.HTTP.Client
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import System.Directory
import System.IO.Strict

import PicSure.Utils.Misc
import PicSure.Types
import PicSure.ConnectionManager

import qualified Data.Binary as B


readConfig :: FilePath -> IO Config
readConfig f = do
  c <- fromJust . decode <$> BSL.readFile f
  manager <- createManager
  return c{manager=manager}


withConfig :: FilePath -> MbStIO a -> IO (Maybe a, PicState)
withConfig s f = do
  config <- readConfig s
  state <- genPicState config
  runStateT (runMaybeT f) state

genPicState c = do
  cache <- case cacheFile c of
    Just file -> do
      doesFileExist file >>= \case
        False -> return cacheRoot
        True -> do
          c <- B.decodeFileOrFail file
          return $ case c of
                     Left _ -> cacheRoot
                     Right cache -> cache
    _ -> return cacheRoot
  return PicState{
    config=c,
    cache = cache
  }
