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
    (Node _ []) -> do
      l <- fmap (unString . PicSure.Utils.Json.lookup "name") . unArray
        <$> listResources
      lift $ do traverse (modify . (flip addToCache)) l
                persistCache
                get >>= liftIO . print 
      return l
    (Node _ l) -> return (map treeValue l)

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
lsPath relative path = let
  l = reverse . snd . foldl f ("", []) $ splitPath $ "/" </> path
    where f (s, acc) e = (s</>e, s</>e : acc)
  go :: [Char] -> MbStIO [String]
  go p = do
    cache <- lift $ gets cache
    cachef <- lift $ cacheFile <$> gets config
    let l = splitPath p
    case treeFind cache l of
      Just l -> liftMaybe . Just $ map (perhaps (not relative) (p</>) . treeValue) l
      Nothing -> do -- not in cache: perform request
        -- paths :: Maybe [String]
        paths <- do
          let pui = (perhaps (not relative) (subStrAfterPath p) . unString . PicSure.Utils.Json.lookup "pui")
          r <- getRequest' (urlPath </> p) >>= MaybeT . return . decode
          return $ (fmap pui . unArray) <$> r
            -- paths = (fmap pui . unArray<$>) <$> (resp>>=decode)
        lift $ traverse (modify . (flip addToCache)) l >> persistCache
        liftMaybe paths
  in last <$> traverse go l

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
  in do ans <- run [startNode]
        error ans

-- |search a specific <node>, starting at the absolute path <from>
searchPath :: String -> String -> MbStIO String
searchPath node from = do
  liftIO $ putStrLn $ "search " ++ node
  let (headNode : restNode) = splitPath node
      checkf = boolToMaybe (==headNode) . pathdirname
  (\e -> foldl (</>) e restNode) <$> bfs lsPath' checkf from >>=  -- bfs
    \p -> (do error p
              lsPath' p) >>=                                           -- check full path existence
    \_ -> liftMaybe $ Just p                                      -- return
  
-- |search in all the available resources
searchPath' node = searchPath node "/"

