{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Lib where

import Network.HTTP.Conduit
import Network.HTTP.Types
-- import qualified Network.URI.Encode as URI
-- import Network.HTTP.Client
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty

import Data.Vector (Vector)
import Data.CaseInsensitive
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.List

import General
import List
import Trees

import Utils
import Types
import Config



urlApi = "rest/v1"
urlResourceService = "resourceService"
urlResources = "resources"
urlPath = "path"

picsureGet :: Config -> String -> RequestHeaders -> IO BSL.ByteString
picsureGet config url args = do
  let fullUrl = domain config </> urlApi </> urlResourceService </> url
--                (foldl' (</>) "". (URI.encode<$>) . splitOn (=='/') $ url) -- encode the provided url
      tok = ("bearer "<>) $ token config
  putStrLn fullUrl
  (responseBody<$>) <$> runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", tok)],
                              responseTimeout = responseTimeoutNone})
             <$> parseUrlThrow fullUrl
    httpLbs req manager

picsureGet' a b = picsureGet a b []

listServices :: Config -> IO [String]
listServices c = do
  resp <- picsureGet' c urlResources
  return $ fmap (unString . Utils.lookup "name") . fromJust . decode $ resp
  -- BSL.putStrLn $ Pretty.encodePretty $ v
  -- return $ extractName . fromJust . decode $ resp

lsPath :: Config -> [Char] -> IO [String]
lsPath c path = do
  resp <- picsureGet' c (urlPath </> path)
  -- return $ Pretty.encodePretty $ (decode resp :: Maybe Value)
  return $ fmap (last . filter (not.isEmpty) . splitOn (=='/') . unString . Utils.lookup "pui")
    . fromJust . decode $ resp

buildPathTree :: Config -> [Char] -> IO (Tree [Char])
buildPathTree c from = lsPath c from >>> print >>= traverse (buildPathTree c . (from</>)) >>= (return . Node from)
-- buildPath c from = do
--   l <- lsPath c from
--   t <- traverse (buildPath c) l
--   return $ Node from t


-- ("fields",Array [
--     Object (fromList [
--                ("<dataTypes>"),
--                ("description",String "By Encounter")])])
--                ("id",Number 1.0),
--                ("name",String "By Encounter"),
--                ("path",String "ENOUNTER"),
--                ("permittedValues",Array [String "YES",String "NO"]),
--                ("relationships",Null),
--                ("required",Bool True),

--  = Just (Array [
--              Object (fromList [
--                         ("predicates",Array [
--                             Object (fromList [
--                                        ("<dataTypes>"),
--                                        ("name",String "CONTAINS"),
--                                        ("defaultPredicate",Bool True),
--                                        ("displayName",String "Contains"),
--                                        ("id",Number 1.0),
--                                        ("description",String "Contains value"),
--                                        ("paths",Array []),
--                                        ("<fields>")
--                                        ]),
--                         ("logicalOperators",Array [String "AND"]),
--                         ("<dataTypes>"),
--                         ("selectOperations",Array []),
--                         ("visualizations",Array []),
--                         ("selectFields",Array []),
--                         ("sorts",Array []),
--                         ("name",String "PMSDN-dev"),
--                         ("relationships",Array [String "PARENT"]),
--                         ("implementation",String "i2b2/tranSMART"),
--                         ("processes",Array []),
--                         ("id",Number 1.0),
--                         ("joins",Array [])])])]


-- datatypes = Array [
--   Object (fromList [
--              ("pattern",String "^(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})$"),
--              ("name",String "dateTime"),
--              ("description",String "Date in yyyy-mm-dd hh:mm:ss format. With hours in 24 hour format")])]
