{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Requester where

import Network.HTTP.Conduit
-- import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.URI.Encode as URI

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Monoid ((<>))
import Control.Monad
import Control.Applicative
import Data.CaseInsensitive

import Data.List (foldl')
import System.IO
import Config
import Utils.Paths
import Utils.List

import ReaderConfig

urlApi = "rest/v1"

load :: String -> ReaderT Config IO String
load s = do
  config <- ask
  return $ token config
  
-- |split url on '/', encode each part, and join them back
encodeUrlPath = foldl' (</>) "" . (URI.encode <$>) . splitOn (=='/')

-- | adds the root path to api for building requests
buildUrl c url = domain c </> urlApi </> encodeUrlPath url

-- |send a GET request to the pic-sure api
-- the provided url will be appened to the root api url,
-- aka https://<domain>/rest/v1/
picsuretest :: String -> RequestHeaders -> ReaderT Config IO (Response BSL.ByteString)
picsuretest url args = do
  config <- ask
  let fullUrl = buildUrl config url
      tok = BS.pack . ("bearer "<>) $ token config -- prepare for GET parameter
  -- putStrLn fullUrl
  runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", tok)],
                              responseTimeout = responseTimeoutNone})
           <$> parseUrlThrow fullUrl
    httpLbs req manager

picsureGetDetailed :: Config -> String -> RequestHeaders -> IO (Response BSL.ByteString)
picsureGetDetailed config url args = do
  let fullUrl = buildUrl config url
      tok = BS.pack . ("bearer "<>) $ token config -- prepare for GET parameter
  -- putStrLn fullUrl
  runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ (\r -> r {requestHeaders = args ++ [("Authorization", tok)],
                              responseTimeout = responseTimeoutNone})
           <$> parseUrlThrow fullUrl
    httpLbs req manager

-- |returns the body of the request encoded with Aeson if all went well.
-- HTTP 500 errors are logged and skipped
-- HTTP 401 (unauthorized) are thrown (token needs to be refreshed)
-- for other errors, wait a bit and retry (for unstable connexions)
picsureGet :: String -> RequestHeaders -> ReaderT Config IO (Maybe [Value])
picsureGet url params = do
  -- we're in the Reader Monad, `ask` gives us the environment, ie the Config value
  c <- ask
  let -- exceptionHandler :: HttpException -> ReaderT Config IO (Maybe [Value])
      exceptionHandler e =
        case e of -- 
          (HttpExceptionRequest _ (StatusCodeException c _)) -> 
            liftIO (putStrLn $ show e) >> f (statusCode . responseStatus $ c)
          _ -> retry e
        where -- f :: Int -> ReaderT Config IO (Maybe [Value])
              f codeNb 
                | codeNb `elem` [500] = liftIO (handleError codeNb) -- throw e -- give up on those error codes
                | codeNb `elem` [401] = throw e
                | otherwise = retry e
                
              -- handleError :: Int -> IO (Maybe [Value])
              handleError n = (appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n")
                              >> print (buildUrl c url) >> print url
                              >> return Nothing
                where fullUrl = buildUrl c url

              -- retry :: HttpException -> ReaderT Config IO (Maybe [Value])
              retry e = liftIO (print e >> threadDelay 1000000) >> picsureGet url params

      -- get :: ReaderT Config IO (Maybe [Value])
      get =  (decode . responseBody <$> picsuretest url params)
      
  (liftCatch catch) get exceptionHandler

-- picsureGet :: String -> RequestHeaders -> ReaderT Config IO (Maybe [Value])
-- picsureGet url params = do -- (decode . responseBody<$>) <$> picsureGetDetailed
--   c <- ask
--   let fullUrl = buildUrl c url
--       handleError n = liftIO ((appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n")
--                       >> print (buildUrl c url) >> print url)
-- -- handleRetry :: a -> ReaderT Config IO (Maybe [Value])
--       handleRetry e = liftIO (print e >> threadDelay 1000000) >> (runReader $ picsureGet url params)
--       -- exceptionHandler e@(HttpExceptionRequest _ (StatusCodeException c _)) =
--       --   (putStrLn $ show e) >> f (statusCode . responseStatus $ c)
--       --   where f codeNb 
--       --           | codeNb `elem` [500] = handleError codeNb >> return Nothing -- throw e -- give up on those error codes
--       --           | codeNb `elem` [401] = throw e
--       --           | otherwise = handleRetry e
--       -- exceptionHandler e = handleRetry e
--       get :: ReaderT Config IO (Maybe [Value])
--       get =  (decode . responseBody <$> picsuretest url params)
--   liftCatch catch $ get handleRetry -- exceptionHandler

-- |send a request without parameters
picsureGet' url = picsureGet url []

picsureGetWith insert url params = picsureGet (insert </> url) params
picsureGetWith' insert url = picsureGetWith insert url []


testRequest :: Config -> String -> RequestHeaders -> IO ()
testRequest c url params = do
  let fullUrl = buildUrl c url
  print fullUrl
  let f = picsureGetDetailed c url params >>= display
        where display r = print r
                >> BSL.putStrLn ( Pretty.encodePretty (decode . responseBody $ r :: Maybe [Value]))
      g e = print (e :: HttpException)
  catch f g
