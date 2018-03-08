{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}
module Lib where

import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Network.URI.Encode as URI
-- import Network.HTTP.Client
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans.Resource

import Control.Exception

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty

import Data.Vector (Vector)
import Data.CaseInsensitive
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.List

import System.Directory

import Utils.General
import Utils.List
import Utils.Trees
import Utils.Json
import Utils.Paths

import Types
import Config


urlApi = "rest/v1"
urlResourceService = "resourceService"
urlResources = "resources"
urlPath = "path"

picsureGet :: Config -> String -> RequestHeaders -> IO BSL.ByteString
picsureGet config url args = do
  let fullUrl = domain config </> urlApi </> urlResourceService </> 
                (foldl' (</>) "". (URI.encode<$>) . splitOn (=='/') $ url) -- encode the provided url
      tok = ("bearer "<>) $ token config
  -- putStrLn fullUrl

  let f = (responseBody<$>) <$> runResourceT $ do
        manager <- liftIO $ newManager tlsManagerSettings
        req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", tok)],
                                  responseTimeout = responseTimeoutNone})
               <$> parseUrlThrow fullUrl
        httpLbs req manager
  catch f (\e -> (putStrLn $ show (e :: HttpException)) >> picsureGet config url args)

picsureGet' a b = picsureGet a b []


listServices :: Config -> IO [String]
listServices c = do
  resp <- picsureGet' c urlResources
  return $ fmap (unString . Utils.Json.lookup "name") . fromJust . decode $ resp

subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

dirname = last . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')

lsPath :: Bool -> Config -> String -> IO [String]
lsPath relative c path = do
  resp <- picsureGet' c (urlPath </> path)
  let f = if relative
          then subStrAfterPath path
          else Prelude.id
  -- return $ Pretty.encodePretty $ (decode resp :: Maybe Value)
  return $ fmap ( f . unString . Utils.Json.lookup "pui")
    . fromJust . decode $ resp


lsPath' = lsPath False

completedFile = lines <$> readFile "data/.completed"

-- buildPathTree :: Config -> [Char] -> IO (Tree [Char])
buildPathTree :: Config -> [Char] -> IO ()
buildPathTree c from = let dirname = "data" </> from
                           isComplete = (flip elem) <$> completedFile
  in print from >>
  not . ($from) <$> isComplete
  >>= (`when` (
        createDirectoryIfMissing True dirname
        >> lsPath True c from
        >>= traverse (buildPathTree c . (from</>))
        >> appendFile "data/.completed" (from++"\n")))

  -- >> (appendFile "tree_" . (take (pathLength from) (repeat ' ')++) $ (dirname from) ++ "\n")


