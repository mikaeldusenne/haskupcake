{-# LANGUAGE OverloadedStrings, BangPatterns, LambdaCase #-}
module PicSure.Resource where

import Control.Monad
import System.Directory
import Data.Foldable

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import Data.List

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


about :: StateT PicState IO BSL.ByteString
about = prettyJson . (>>=decodeValue) <$> getRequest' urlAbout


-- |list available services on pic-sure
listServices :: StateT PicState IO [String]
listServices = do
  gets cache >>= \case
    (Node _ []) -> do
      l <- fmap (unString . PicSure.Utils.Json.lookup "name") . unArray
        <$> listResources
      traverse (modify . (flip addToCache)) l
      get >>= liftIO . print 
      return l
    (Node _ l) -> return (map treeValue l)

listResources = decodeValue' <$> getRequest' urlResources

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathdirname = (\l -> if isEmpty l then "" else last l) . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')

  
-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
-- lsPath with the empty string switches to lsResources
lsPath :: Bool -> String -> StateT PicState IO [String]
lsPath _ "" = listServices
lsPath relative path = let
  l = reverse . snd . foldl f ("", []) $ splitPath path
    where f (s, acc) e = (s</>e, s</>e : acc)
  go p = do
    cache <- gets cache
    let l = splitPath p
        found = treeFind cache l    
    case found of
      Nothing -> do
        resp <- getRequest' (urlPath </> p)
        let pui = ( f . unString . PicSure.Utils.Json.lookup "pui")
              where f = if relative then subStrAfterPath p else Prelude.id
            paths = case resp of Nothing -> []
                                 r -> fmap pui . unArray . decodeValue' $ r
        traverse (modify . (flip addToCache)) paths
        return paths
      Just l -> return $ map (f . treeValue) l
        where f = if relative then id else (p</>)
  in last <$> traverse go l

lsPath' = lsPath False


findPath term = do
  resp <- getRequest urlFind [("term", term)]
  return resp

-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs :: (String -> StateT PicState IO [String])
    -> (String -> Maybe String)
    -> String
    -> StateT PicState IO (Maybe String)
-- todo cleaner / more abstract types?
bfs nextf checkf startNode = let
  run :: [String] -> StateT PicState IO (Maybe String)
  run [] = return Nothing  -- found nothing
  run (node : nodes) = do
        liftIO $ putStrLn node
        nexts <- nextf node
        let f acc e = acc `mplus` checkf e
        case (foldl f Nothing nexts) of
          Nothing -> run $ nodes++nexts
          n -> return $ (node</>) <$> n
  in run [startNode]

-- |search a specific <node>, starting at the absolute path <from>
searchPath :: String -> String -> StateT PicState IO (Maybe String)
searchPath node from = do
  let (headNode : restNode) =  splitPath node
      checkf = ((\n -> if n == headNode then Just n else Nothing) . pathdirname)
  p <- case checkf from of
    Nothing -> do c <- gets config
                  bfs (lsPath') checkf from
    n -> do
      
      liftIO $ do
        print headNode
        print restNode
        print n
      return n -- . Just $ foldl (</>) n restNode
  return $ (\e -> foldl (</>) e restNode) <$> p
-- |search in all the available resources
searchPath' node = searchPath node ""

-------- tests --------

completedFile = "data/.completed"

-- -- |reproduces the data tree in the file system by creating a directory for each item
-- -- this is for testing purposes, the amount of HTTP requests needed if way to high and
-- -- this takes ages to complete.
buildPathTree :: [Char] -> StateT PicState IO ()
buildPathTree fromNode = do
  let go !completed node = do -- bangpattern needed to enforce strictness of reading (file locked error)
        let dirname = "data" </> node
            isNotComplete = True -- not $ elem node completed
        liftIO $ putStrLn ((take (pathLength node - 1) $ repeat ' ') ++ show (pathLength node) ++ (pathdirname node)) -- just being fancy
        when ((pathLength node <= 18) && isNotComplete) $ do
          liftIO $ createDirectoryIfMissing True dirname
          lsPath True node >>= traverse_ (go completed . (node</>))
          -- >> appendFile completedFile (node++"\n")
          -- >> hPutStrLn h (node ++ "\n"))
          -- >>= (return . (node:) . concat))  
  completed <- lines <$> (liftIO $ readFile completedFile)
  go completed fromNode
