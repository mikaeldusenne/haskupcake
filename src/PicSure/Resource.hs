{-# LANGUAGE OverloadedStrings, BangPatterns, LambdaCase, DuplicateRecordFields #-}
module PicSure.Resource where

import Control.Monad

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State

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

about :: PicSureM BSL.ByteString
about = getRequest' urlAbout >>= (return . prettyJson . fromJust .  decodeValue)

-- |list available services on pic-sure
listServices :: PicSureM [String]
listServices = do
  gets cache >>= \case
    (Node _ l@(_:_)) -> return (map treeValue l)
    _ -> do
      l <- fmap (unString . PicSure.Utils.Json.lookup "name") . unArray
        <$> listResources
      do traverse (modify . (flip addToCache)) l
         persistCache
      return l

listResources :: PicSureM Value
listResources = getRequest' urlResources >>= return . fromJust . decodeValue

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathbasename = (\l -> if isEmpty l then "" else last l) . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')


-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
-- lsPath with the empty string switches to lsResources
lsPath :: Bool -> String -> PicSureM [String]
lsPath _ "" = listServices
lsPath relative path = do
  let l = [path]
      go :: [Char] -> PicSureM [String]
      go p = do
        cacheFetch p >>= \case
          (Just (Node _ ll)) -> do
            -- liftIO $ print "found in cache"
            return . (map (perhaps (not relative) (p</>) . treeValue)) $ ll
          _ -> do -- not in cache: perform request
            -- liftIO $ print "lspath"
            paths <- do
              let pui = (perhaps relative (subStrAfterPath p) . unString . PicSure.Utils.Json.lookup "pui")
              r <- fromJust . decode <$> getRequest' (urlPath </> p)
              return $ (fmap pui . unArray) $ r
            -- liftIO $ print ("paths", paths)
            (case paths of (_:_) -> mapM_ (modify . (flip addToCache)) paths
                           [] -> modify ((flip setNoChildrenCache) path)) >> persistCache
            return paths

  last <$> traverse go l

lsPath' = lsPath False


findPath term = getRequest urlFind [("term", term)]


-- | returns a list of leafs starting at the specified subtree
-- | warning: for categorical variables it will return one leaf for each modality
find_leafs :: String -> PicSureM [String]
find_leafs "" = liftMaybe Nothing
find_leafs path = do
  children <- lsPath' path
  if isEmpty children
    then liftMaybe $ Just [path]
    else do l <- traverse find_leafs children
            liftMaybe . Just $ concat l


-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs :: (String -> PicSureM [String])
    -> (String -> Maybe String)
    -> String
    -> PicSureM String
bfs nextf checkf startNode = let
  run :: [String] -> PicSureM String
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
searchPath :: String -> String -> PicSureM String
searchPath node from = 
  isAbsolute node >>= ((?) (liftMaybe (Just node)) $ do
  let (headNode : restNode) = splitPath node
      checkf = boolToMaybe (==headNode) . pathbasename
  (\e -> foldl (</>) e restNode) <$> bfs lsPath' checkf from >>=  -- bfs
    \p -> (do l <- lsPath' (dirname p)
              liftMaybe . boolToMaybe (p `elem`) $ l 
          )  
               >>=                                           -- check full path existence
    \_ -> liftMaybe $ Just p)                                      -- return
  
-- |search in all the available resources
searchPath' node = searchPath node "/"

isAbsolute :: String -> PicSureM Bool
isAbsolute path = any f <$> listServices
  where f service = isPrefixOf ("/"</>service) ("/"</>path) 
