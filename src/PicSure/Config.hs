module PicSure.Config where

import Network.HTTP.Conduit
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.Trans.Reader
-- import Control.Monad.IO.Class (liftIO)

import PicSure.Utils.Misc
import PicSure.Types
import PicSure.Security

readConfig :: FilePath -> IO Config
readConfig f = do
  c <- fromJust . decode <$> BSL.readFile f
  case auth c of
    ApiKey k -> do
      cj <- runReaderT startSession c
      return c{sessionCookies=cj}
    _ -> return c
