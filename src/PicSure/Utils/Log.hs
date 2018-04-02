{-# LANGUAGE LambdaCase #-}
module PicSure.Utils.Log where

import Control.Monad.Trans.State
-- import Data.Time.Clock
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BSL

import PicSure.Types

data LogLevel = DEBUG | WARN

printLog lvl = putStrLnLog lvl . BSL.pack . show

putStrLnLog :: LogLevel -> BSL.ByteString -> PicSureM ()
putStrLnLog lvl e = let disp = liftIO $ BSL.putStrLn e
  in (debug <$> gets config) >>= \case
  True -> disp
  False -> case lvl of WARN -> disp
                       _ -> return ()
