{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, BangPatterns #-}
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
import Control.Concurrent (threadDelay)
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
import System.IO

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

-- |send a GET request to the pic-sure api.
-- returns the body of the request if all went well.
-- HTTP 500 errors are logged and skipped
-- HTTP 401 (unauthorized) are thrown (token needs to be refreshed)
-- for other errors, wait a bit and retry (for unstable connexions)
picsureGet :: Config -> String -> RequestHeaders -> IO (Maybe [Value])
picsureGet config url args = do
  let encodeUrlPath = foldl' (</>) "". (URI.encode <$>) . splitOn (=='/')

  let fullUrl = domain config </> urlApi </> urlResourceService </> encodeUrlPath url
      tok = BS.pack . ("bearer "<>) $ token config -- prepare for GET parameter
  -- putStrLn fullUrl

  let handleError n = (appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n")
                      >> print fullUrl >> print url
      handleRetry e = print e >> threadDelay 1000000 >> picsureGet config url args
      exceptionHandler e@(HttpExceptionRequest _ (StatusCodeException c _)) =
        (putStrLn $ show e) >> f (statusCode . responseStatus $ c)
        where f codeNb 
                | codeNb `elem` [500] = handleError codeNb >> return Nothing -- throw e -- give up on those error codes
                | codeNb `elem` [401] = throw e
                | otherwise = handleRetry e
      exceptionHandler e = handleRetry e

      f = (decode . responseBody<$>) <$> runResourceT $ do
        manager <- liftIO $ newManager tlsManagerSettings
        req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", tok)],
                                  responseTimeout = responseTimeoutNone})
               <$> parseUrlThrow fullUrl
        httpLbs req manager
  catch f exceptionHandler

-- |send a request without parameters
picsureGet' a b = picsureGet a b []

-- |list available services on pic-sure
listServices :: Config -> IO [String]
listServices c = do
  resp <- picsureGet' c urlResources
  return $ fmap (unString . Utils.Json.lookup "name") $ fromJust resp

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathdirname = last . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')

-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
lsPath :: Bool -> Config -> String -> IO [String]
lsPath relative c path = do
  resp <- picsureGet' c (urlPath </> path)
  let pui = ( f . unString . Utils.Json.lookup "pui")
        where f = if relative then subStrAfterPath path else Prelude.id
  return $ case resp of Nothing -> []
                        Just r -> fmap pui $ r

lsPath' = lsPath False

completedFile = "data/.completed"


-- TODO conduit / streaming
-- or pure path building and then IO actions, but then we need to handle all the possible errors
-- |reproduces the data tree in the file system by creating a directory for each item
-- this is for testing purposes, the amount of HTTP requests needed if way to high and
-- this takes ages to complete.
buildPathTree :: Config -> [Char] -> IO ()
buildPathTree c fromNode = do
  let go !completed node = let -- bangpattern needed to enforce strictness of reading (file locked error)
        dirname = "data" </> node
        isNotComplete = not $ elem node completed
        in putStrLn ((take (pathLength node - 1) $ repeat ' ') ++ (pathdirname node)) >> -- just being fancy
           when isNotComplete (
               createDirectoryIfMissing True dirname
               >> lsPath True c node
               >>= traverse (go completed . (node</>))
               >> appendFile completedFile (node++"\n"))
               -- >> hPutStrLn h (node ++ "\n"))
               -- >>= (return . (node:) . concat))
     
  completed <- lines <$> readFile completedFile
  go completed fromNode
  

  -- >> (appendFile "tree_" . (take (pathLength from) (repeat ' ')++) $ (dirname from) ++ "\n")
