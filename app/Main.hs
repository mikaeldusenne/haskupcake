{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Main where

import Network.HTTP.Conduit
-- import Network.HTTP.Client
import GHC.Generics

import Lib
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import Data.Aeson

import Data.HashMap.Strict
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List
import Data.CaseInsensitive
-- import System.FilePath.Posix
import Data.Monoid
import General
-- import List

-- import Types



urlApi = "/rest/v1"
urlResourceService = "/resourceService"
urlResources = "/resources"

token :: IO BS.ByteString
token = ("bearer "<>) <$> BS.readFile "token"

urlDomain = "https://pmsdn-dev.hms.harvard.edu"

(</>) a b = let n = f isSuffixOf a + f isInfixOf b
                f g e = if g "/" e then 1 else 0
            in case n of 0 -> a ++ '/' : b
                         1 -> a ++ b
                         2 -> a ++ tail b

picsureGet :: String -> [(CI BS.ByteString, BS.ByteString)]
           -> IO (Response BSL.ByteString)
picsureGet url args = do
  let fullUrl = urlDomain </> urlApi </> url
  -- print fullUrl
  runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", token)]})
             <$> parseUrlThrow fullUrl
    httpLbs req manager
  
listServices = (extractName . fromJust . decode . responseBody )
                <$> picsureGet (urlResourceService </> urlResources) []

main :: IO ()
main = listServices >>= putStrLn
  -- -- this works:
  -- let (Just json) = (decode (responseBody res) :: Maybe Value)

extractName (ResName [RN {name = name}]) = name

data ResName = ResName [RN]
  deriving (Show, Generic)

data RN = RN {name :: String}
  deriving (Show, Generic)

instance FromJSON ResName

instance FromJSON RN where
    parseJSON (Object v) = do
        name <- v .: "name"
        return $ RN { name = name }
    parseJSON e = error $ show e
