module PicSure.Config where

import Network.HTTP.Client
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.Trans.Reader
-- import Control.Monad.IO.Class (liftIO)

import PicSure.Utils.Misc
import PicSure.Types
import PicSure.Security
import PicSure.ConnexionManager


readConfig :: FilePath -> IO Config
readConfig f = do
  c <- fromJust . decode <$> BSL.readFile f
  manager <- createManager
  let c' = c{manager=manager}
  case auth c of
    ApiKey k -> do
      cj <- runReaderT startSession c'
      return c'{sessionCookies=cj}
    _ -> return c'
