{-# LANGUAGE OverloadedStrings #-}
module PicSure.Requester where

import Network.HTTP.Conduit
-- import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.URI.Encode as URI

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
-- import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Monoid ((<>))
import Control.Monad
import Control.Applicative
import Data.CaseInsensitive
import Data.List

import Data.List (foldl')
import System.IO

import PicSure.Config
import PicSure.Utils.Paths
import PicSure.Utils.General
import PicSure.Utils.List

urlApi = "rest/v1"

load :: String -> ReaderT Config IO String
load s = do
  config <- ask
  return $ token config
  
-- |split url on '/', encode each part, and join them back
encodeUrlPath = foldl' (</>) "" . (URI.encode <$>) . splitOn (=='/')

-- | adds the root path to api for building requests
buildUrl url = ((</> (urlApi </> encodeUrlPath url)) . domain) <$> ask

-- |a better 'show' function for Request objects
logRequest req = do
  putStrLn "────────────────"
  putStrLn . nice'title . BS.unpack
    $ BS.concat ["requesting ", method req, " ", host req, path req, queryString req]
  putStrLn $ if method req == "GET"
             then "headers: " ++ show (requestHeaders req)
             else "body: \n" ++
                  (\(RequestBodyLBS e) -> BSL.unpack . Pretty.encodePretty . fromJust $ (decode e :: Maybe Value))
                  (requestBody req)
  putStrLn "────────────────"


-- |No http error handling
picsureRequest' :: String -> (Request -> Request) -> ReaderT Config IO (Response BSL.ByteString)
picsureRequest' url buildf = do
  config <- ask
  fullUrl <- buildUrl url
  let tok = BS.pack . ("bearer "<>) $ token config -- prepare for GET parameter
      setRequestOptions r = r {requestHeaders = requestHeaders r ++ [("Authorization", tok)],
                               responseTimeout = responseTimeoutNone}
  runResourceT $ do
    manager <- liftIO $ newManager tlsManagerSettings
    req <- liftIO $ setRequestOptions . buildf <$> parseUrlThrow fullUrl
    when (debug config) . liftIO $ logRequest req
    -- liftIO $ (\(RequestBodyLBS s) -> BSL.putStrLn s) $ requestBody req
    httpLbs req manager


-- |returns the body of the request encoded with Aeson if all went well.
-- HTTP 500 errors are logged and skipped
-- HTTP 401 (unauthorized) are thrown (token needs to be refreshed)
-- for other errors, wait a bit and retry (for unstable connexions)
picsureRequest :: String -> (Request -> Request) -> ReaderT Config IO (Maybe Value)
picsureRequest url buildf = do
  -- we're in the Reader Monad, `ask` gives us the environment, ie the Config value
  c <- ask
  fullUrl <- buildUrl url
  let -- exceptionHandler :: HttpException -> ReaderT Config IO (Maybe [Value])
      exceptionHandler e =
        case e of -- 
          (HttpExceptionRequest _ (StatusCodeException c b)) -> 
            liftIO (putStrLn . concat . afterEach "\n────────────────────────\n" $ [show e,BS.unpack b])
            >> f (statusCode . responseStatus $ c)
          _ -> retry e
        where -- f :: Int -> ReaderT Config IO (Maybe [Value])
              f codeNb 
                | codeNb `elem` [500] = liftIO (handleError codeNb) -- throw e -- give up on those error codes
                | codeNb `elem` [401] = throw e
                | otherwise = retry e
                
              -- handleError :: Int -> IO (Maybe [Value])
              handleError n = -- (appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n") >>
                              print fullUrl >> print url >>
                              return Nothing

              -- retry :: HttpException -> ReaderT Config IO (Maybe [Value])
              retry e = liftIO (print e >> print "retrying soon..." >> threadDelay 1000000) >> picsureRequest url buildf

      -- get :: ReaderT Config IO (Maybe [Value])
      get = decode . responseBody <$> picsureRequest' url buildf
      
  (liftCatch catch) get exceptionHandler

-- |send a GET request to the pic-sure api
-- the provided url will be appened to the root api url,
-- aka https://<domain>/rest/v1/
picsureGetRequest :: String -> RequestHeaders -> ReaderT Config IO (Maybe Value)
picsureGetRequest url params = do
  picsureRequest url (\r -> r {requestHeaders = params})

-- |POST request
picsurePostRequest :: String -> BSL.ByteString -> ReaderT Config IO (Maybe Value)
-- todo: unify GET / POST functions with the use of Request directly instead of params / body arguments
picsurePostRequest url body = do
  picsureRequest url (\r -> r{method = "POST",
                              requestBody = RequestBodyLBS body})

-- |send a request without parameters
picsureGetRequest' url = picsureGetRequest url []
