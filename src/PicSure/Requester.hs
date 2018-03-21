{-# LANGUAGE OverloadedStrings #-}
module PicSure.Requester where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
-- import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.URI
import qualified Network.URI.Encode as URI

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
-- import Data.Conduit.Binary (sinkFile)
import Control.Monad.IO.Class
-- import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Monoid ((<>))
import Control.Monad
import Control.Applicative
import qualified Data.CaseInsensitive as CI
import Data.List

import Data.List (foldl')
import System.IO

import PicSure.Utils.Misc
import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Utils.Json
import PicSure.Types



urlApi = "rest/v1"

-- |split url on '/', encode each part, and join them back
encodeUrlPath = foldl' (</>) "" . (URI.encode <$>) . splitOn (=='/')

-- | adds the root path to api for building requests
-- buildUrl url = ((</> (urlApi </> encodeUrlPath url)) . domain) <$> ask

-- |a better 'show' function for Request objects
logRequest req = do
  putStrLn "────────────────"
  putStrLn . nice'title
    . (("requesting " ++ (BS.unpack $ method req)) ++) . (" "++)
    . foldl (</>) "" $ map (BS.unpack) [ host req, path req, queryString req]
  when (method req /= "GET") $ putStrLn $ 
    "body: \n" ++
    (\(RequestBodyLBS e) -> BSL.unpack . prettyJson . (decode :: BSL.ByteString -> Maybe Value) $ e)
    (requestBody req)
  putStrLn "────────────────"


-- |No http error handling
-- request' :: String -> PostGet -> ReaderT Config IO (Response BSL.ByteString)
request' :: String -> PostGet -> (Response BodyReader -> IO b) -> ReaderT Config IO b
request' url postget action = do
  config <- ask
  let tokparam = [("Authorization", BS.pack ("bearer " <> (runToken $ auth config)))]
      applyPostGet req = case postget of
        (Params l) -> req{ queryString = (rq $ l)
                         , method = "GET"}
        (Body   b) -> req{ requestBody = b, method = "POST"}
        where rq = renderQuery True . map (applyToSnd Just)
  liftIO $ do
    req <- applyPostGet . (\r -> r
          {requestHeaders=tokparam,
           path = BS.pack $ urlApi </> encodeUrlPath url,
           port = 443,
           secure = True}) <$> (parseUrlThrow $ domain config)
    when (debug config) . liftIO $ logRequest req
    withResponse req (manager config) action
    -- resp <- httpLbs req $ manager config
    -- responseClose resp -- needed?
    -- return $ resp


-- |returns the body of the request encoded with Aeson if all went well.
-- HTTP 500 errors are logged and skipped
-- HTTP 401 (unauthorized) are thrown (token needs to be refreshed)
-- for other errors, wait a bit and retry (for unstable connexions)
request :: String -> PostGet -> ReaderT Config IO (Maybe BSL.ByteString)
request url postget = do
  -- we're in the Reader Monad, `ask` gives us the environment, ie the Config value
  c <- ask
  let -- exceptionHandler :: HttpException -> ReaderT Config IO (Maybe BSL.ByteString)
      exceptionHandler e =
        case e of -- 
          (HttpExceptionRequest _ (StatusCodeException c b)) -> 
            liftIO (putStrLn . concat . afterEach "\n────────────────────────\n" $ [show e,BS.unpack b])
            >> f (statusCode . responseStatus $ c)
          _ -> retry e
        where -- f :: Int -> ReaderT Config IO (Maybe BSL.ByteString)
              f codeNb
                | codeNb `elem` [500] = liftIO (handleError codeNb) -- throw e -- give up on those error codes
                | codeNb `elem` [401, 404] = throw e
                | otherwise = retry e
                
              -- handleError :: Int -> IO (Maybe [Value])
              handleError n = -- (appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n") >>
                              print (domain c) >> print url >>
                              return Nothing

              -- retry :: HttpException -> ReaderT Config IO (Maybe [Value])
              retry e = liftIO (print e >> print "retrying soon..." >> threadDelay 1000000) >> request url postget

      get :: ReaderT Config IO (Maybe BSL.ByteString)
      get = Just . BSL.fromChunks <$> request' url postget (brConsume . responseBody)
  (liftCatch catch) get exceptionHandler

-- |send a GET request to the pic-sure api
-- the provided url will be appened to the root api url,
-- aka https://<domain>/rest/v1/
getRequest :: String -> [(BS.ByteString, BS.ByteString)] -> ReaderT Config IO (Maybe BSL.ByteString)
getRequest url params = request url (Params params)

-- |POST request
postRequest :: String -> BSL.ByteString -> ReaderT Config IO (Maybe BSL.ByteString)
postRequest url body = request url (Body $ RequestBodyLBS body)

-- |send a request without parameters
getRequest' url = getRequest url []

