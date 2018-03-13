{-# LANGUAGE OverloadedStrings #-}
module PicSure.Security (
  createKey, startSession, endSession, createState) where

import Control.Monad.Trans.Reader

import qualified Data.ByteString.Char8 as BS
import PicSure.Requester
import PicSure.Utils.Paths
import PicSure.Config

urlSecurityService = "securityService"

f = getRequest' . (urlSecurityService</>)

createKey     = f "createKey"
createState   = f "createState"
endSession    = f "endSession"
startSession  = do
  key <- auth <$> ask
  case key of
    Token _ -> error "please use the token, no need to use startSession"
    ApiKey k -> getRequest "startSession" [("key", BS.pack k)]
