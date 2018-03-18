{-# LANGUAGE OverloadedStrings #-}
module PicSure.Requester where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
-- import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Types.URI
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
import qualified Data.CaseInsensitive as CI
import Data.List

import Data.List (foldl')
import System.IO

import PicSure.Utils.Misc
import PicSure.Utils.Paths
import PicSure.Utils.List
import PicSure.Types

data PostGet = Params [(BS.ByteString, BS.ByteString)] | Body RequestBody

urlApi = "rest/v1"

-- |split url on '/', encode each part, and join them back
encodeUrlPath = foldl' (</>) "" . (URI.encode <$>) . splitOn (=='/')

-- | adds the root path to api for building requests
-- buildUrl url = ((</> (urlApi </> encodeUrlPath url)) . domain) <$> ask

-- |a better 'show' function for Request objects
logRequest req = do
  putStrLn "────────────────"
  putStrLn . nice'title . BS.unpack
    $ BS.concat ["requesting ", method req, " ", host req, path req, queryString req]
  when (method req /= "GET") $ putStrLn $ 
    "body: \n" ++
    (\(RequestBodyLBS e) -> BSL.unpack . Pretty.encodePretty . fromJust $ (decode e :: Maybe Value))
    (requestBody req)
  putStrLn "────────────────"


-- |No http error handling
request' :: String -> PostGet -> ReaderT Config IO (Response BSL.ByteString)
request' url postget = do
  config <- ask
  -- fullUrl <- buildUrl url
  liftIO $ print config
  let tokparam = case auth config of
        Token t -> [("Authorization", BS.pack ("bearer " <> t))]
        _ -> []
        -- ApiKey k -> [("key", BS.pack k)]
      -- setRequestOptions r = r {queryString = renderQuery True . map (applyToSnd Just) $ tokparam ++ (requestHeaders r),
      --                          responseTimeout = responseTimeoutNone}
      applyPostGet req = case postget of
        (Params l) -> req{ queryString = (rq $ l)
                         --  `BS.append` cookies
                         , method = "GET"}
        (Body   b) -> req{ requestBody = b,       method = "POST"}
        where rq = renderQuery True . map (applyToSnd Just)
              -- cookies :: BS.ByteString
              -- cookies = case sessionCookies config of
              --             Nothing -> mempty
              --             Just jar -> BS.pack . intercalate "&" . map ("Cookie: "++) $ map (\(a, b) -> a ++ "=" ++ b) $ bs
  liftIO $ do
    req <- applyPostGet . (\r -> r
          {requestHeaders=tokparam,
           cookieJar = sessionCookies config,
           path = BS.pack $ urlApi </> encodeUrlPath url,
           port = 443,
           secure = True}) <$> (parseUrlThrow $ domain config)
    when (debug config) . liftIO $ logRequest req
    -- liftIO $ (\(RequestBodyLBS s) -> BSL.putStrLn s) $ requestBody req
    httpLbs req $ manager config


-- |returns the body of the request encoded with Aeson if all went well.
-- HTTP 500 errors are logged and skipped
-- HTTP 401 (unauthorized) are thrown (token needs to be refreshed)
-- for other errors, wait a bit and retry (for unstable connexions)
request :: String -> PostGet -> ReaderT Config IO (Maybe Value)
request url postget = do
  -- we're in the Reader Monad, `ask` gives us the environment, ie the Config value
  c <- ask
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
                | codeNb `elem` [401, 404] = throw e
                | otherwise = retry e
                
              -- handleError :: Int -> IO (Maybe [Value])
              handleError n = -- (appendFile "logs" $ show n ++ "," ++ show url ++ "," ++ show fullUrl ++ "\n") >>
                              print (domain c) >> print url >>
                              return Nothing

              -- retry :: HttpException -> ReaderT Config IO (Maybe [Value])
              retry e = liftIO (print e >> print "retrying soon..." >> threadDelay 1000000) >> request url postget

      -- get :: ReaderT Config IO (Maybe [Value])
      get = decode . responseBody <$> request' url postget
      
  (liftCatch catch) get exceptionHandler

-- |send a GET request to the pic-sure api
-- the provided url will be appened to the root api url,
-- aka https://<domain>/rest/v1/
getRequest :: String -> [(BS.ByteString, BS.ByteString)] -> ReaderT Config IO (Maybe Value)
getRequest url params = request url (Params params)

-- |POST request
postRequest :: String -> BSL.ByteString -> ReaderT Config IO (Maybe Value)
postRequest url body = request url (Body $ RequestBodyLBS body)

-- |send a request without parameters
getRequest' url = getRequest url []

