{-# LANGUAGE OverloadedStrings, BangPatterns, LambdaCase, DuplicateRecordFields #-}
module PicSure.Resource where

import Control.Monad
import System.Directory
import Data.Foldable

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Data.List
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BSL

import PicSure.Utils.Misc
import PicSure.Utils.List
import PicSure.Utils.Trees
import PicSure.Utils.Json
import PicSure.Cache
import PicSure.Utils.Paths
import PicSure.Types
import PicSure.Requester

urlResourceService = "resourceService"
urlResources = urlResourceService </> "resources"
urlPath = urlResourceService </> "path"
urlFind = urlResourceService </> "find"
urlSystemService = "systemService"
urlAbout = urlSystemService </> "about"

about :: MbStIO BSL.ByteString
about = getRequest' urlAbout >>= (MaybeT . return . (prettyJson <$>) .  decodeValue)

-- |list available services on pic-sure
listServices :: MbStIO [String]
listServices = do
  lift (gets cache) >>= \case
    (Node _ l@(x:xs)) -> return (map treeValue l)
    _ -> do
      l <- fmap (unString . PicSure.Utils.Json.lookup "name") . unArray
        <$> listResources
      lift $ do traverse (modify . (flip addToCache)) l
                persistCache
      return l

listResources :: MbStIO Value
listResources = getRequest' urlResources >>= MaybeT . return . decodeValue

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathdirname = (\l -> if isEmpty l then "" else last l) . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')


-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
-- lsPath with the empty string switches to lsResources
lsPath :: Bool -> String -> MbStIO [String]
lsPath _ "" = listServices
lsPath relative path = do
  cachef <- lift $ cacheFile <$> gets config
  let l = [path]
      -- l = perhaps (cachef==Nothing) ((:[]).last) . reverse . snd . foldl f ("", []) $ splitPath $ path
      --   where f (s, acc) e = (s</>e, s</>e : acc)
      go :: [Char] -> MbStIO [String]
      go p = do
        lift (cacheFetch p) >>= \case
          Just ll -> liftMaybe . Just $ map (perhaps (not relative) (p</>) . treeValue) ll
          Nothing -> do -- not in cache: perform request
            paths <- do
              let pui = (perhaps relative (subStrAfterPath p) . unString . PicSure.Utils.Json.lookup "pui")
              r <- getRequest' (urlPath </> p) >>= MaybeT . return . decode
              return $ (fmap pui . unArray) <$> r
            lift (when (paths/=Nothing) $ traverse (modify . (flip addToCache)) (fromJust paths) >> persistCache)
            liftMaybe paths

  last <$> traverse go l

lsPath' = lsPath False


findPath term = getRequest urlFind [("term", term)]


-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs :: (String -> MbStIO [String])
    -> (String -> Maybe String)
    -> String
    -> MbStIO String
-- todo cleaner / more abstract types?
bfs nextf checkf startNode = let
  run :: [String] -> MbStIO String
  run [] = liftMaybe Nothing  -- found nothing
  run (node : nodes) = do
        liftIO $ putStrLn node
        nexts <- nextf node
        let f acc e = acc `mplus` checkf e
        case (foldl f Nothing (node:nexts)) of
          Nothing -> run $ nodes++nexts
          n -> liftMaybe $ (node</>) <$> n
  in run [startNode]

-- |search a specific <node>, starting at the absolute path <from>
searchPath :: String -> String -> MbStIO String
searchPath node from = 
  isAbsolute node >>= ((?) (liftMaybe (Just node)) $ do
  let (headNode : restNode) = splitPath node
      checkf = boolToMaybe (==headNode) . pathdirname
  (\e -> foldl (</>) e restNode) <$> bfs lsPath' checkf from >>=  -- bfs
    \p -> lsPath' p >>=                                           -- check full path existence
    \_ -> liftMaybe $ Just p)                                      -- return
  
-- |search in all the available resources
searchPath' node = searchPath node "/"

isAbsolute :: String -> MbStIO Bool
isAbsolute path = any f <$> listServices
  where f service = isPrefixOf ("/"</>service) ("/"</>path) 
