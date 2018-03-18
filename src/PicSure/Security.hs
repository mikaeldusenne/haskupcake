{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module PicSure.Security (
  createKey, startSession, endSession, createState) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)

import Network.HTTP.Client
import Network.HTTP.Types(Header)

import qualified Data.ByteString.Char8 as BS
import PicSure.Requester
import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Types
import Data.Time.Clock

urlSecurityService = "securityService"

f = getRequest' . (urlSecurityService</>)

createKey     = f "createKey"
createState   = f "createState"
endSession    = f "endSession"

-- startSession :: ReaderT Config IO b
startSession :: ReaderT Config IO (Maybe CookieJar)
startSession  = do
  time <- liftIO getCurrentTime
  -- return Nothing
  -- liftIO $ putStrLn "Authenticating with API key..."
  
  (auth <$> ask) >>= \case
    Token _ -> return Nothing
    ApiKey k -> do
      resp <- request' (urlSecurityService</>"startSession") (Params [("key", BS.pack k)])
      liftIO $ print resp
      let cj = responseCookieJar resp
      -- let cookies = map ((read :: String -> Cookie). BS.unpack .snd) . filter ((=="Set-Cookie") . fst) $ responseHeaders resp
      return . Just $ cj

