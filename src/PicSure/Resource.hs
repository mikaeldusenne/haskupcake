{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module PicSure.Resource where

import Control.Monad
import System.Directory
import Data.Foldable

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

import PicSure.Utils.Misc
import PicSure.Utils.List
import PicSure.Utils.Trees
import PicSure.Utils.Json
import PicSure.Utils.Paths
import PicSure.Types
import PicSure.Requester

urlResourceService = "resourceService"
urlResources = urlResourceService </> "resources"
urlPath = urlResourceService </> "path"
urlFind = urlResourceService </> "find"


-- |list available services on pic-sure
listServices :: ReaderT Config IO [String]
listServices = fmap (unString . PicSure.Utils.Json.lookup "name") . unArray . fromJust
               <$> listResources

listResources = getRequest' urlResources

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathdirname = (\l -> if isEmpty l then "" else last l) . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')

-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
-- lsPath with the empty string switches to lsResources
lsPath :: Bool -> String -> ReaderT Config IO [String]
lsPath _ "" = listServices
lsPath relative path = do
  resp <- getRequest' (urlPath </> path)
  let pui = ( f . unString . PicSure.Utils.Json.lookup "pui")
        where f = if relative then subStrAfterPath path else Prelude.id
  return $ case resp of Nothing -> []
                        Just r -> fmap pui . unArray $ r

lsPath' = lsPath False


findPath term = do
  resp <- getRequest urlFind [("term", term)]
  return resp

-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs :: (String -> IO [String]) -> (String -> Maybe String) -> String -> IO (Maybe String)
-- todo cleaner / more abstract types?
bfs nextf checkf startNode = let
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
searchPath :: String -> String -> ReaderT Config IO (Maybe String)
searchPath node from = do
  let checkf = ((\n -> if n == node then Just n else Nothing) . pathdirname)
  case checkf from of
    Nothing -> do c <- ask
                  liftIO $ bfs ((`runReaderT` c) . lsPath') checkf from
    n -> return n

-- |search in all the available resources
searchPath' node = searchPath node ""

-------- tests --------

completedFile = "data/.completed"

-- -- |reproduces the data tree in the file system by creating a directory for each item
-- -- this is for testing purposes, the amount of HTTP requests needed if way to high and
-- -- this takes ages to complete.
buildPathTree :: [Char] -> ReaderT Config IO ()
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
