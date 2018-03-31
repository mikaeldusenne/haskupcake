{-# LANGUAGE LambdaCase #-}
module PicSure.Config where
import Data.Yaml
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Trans.State

import System.Directory

import PicSure.Utils.Misc
import PicSure.Types
import PicSure.ConnectionManager

import qualified Data.Binary as B


readConfig :: FilePath -> IO Config
readConfig f = do
  c <- fromJust . decode <$> BS.readFile f
  manager <- createManager
  return c{manager=manager}


withConfig :: FilePath -> PicSureM a -> IO a
withConfig s f = readConfig s >>=
                 genPicState  >>=
                 (fst <$>) . runStateT f

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
