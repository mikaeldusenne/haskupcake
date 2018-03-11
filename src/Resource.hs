{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Resource where

import Control.Monad
import System.Directory

import Utils.General
import Utils.List
import Utils.Trees
import Utils.Json
import Utils.Paths
import Config
import Requester

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

urlResourceService = "resourceService"
urlResources = "resources"
urlPath = "path"
urlFind = "find"


picsureGetService = picsureGetWith urlResourceService
picsureGetService' url = picsureGetService url []

-- |list available services on pic-sure
listServices :: ReaderT Config IO [String]
listServices = fmap (unString . Utils.Json.lookup "name") . fromJust
               <$> picsureGetService' urlResources

-- todo debug this
subStrAfterPath path = drop (length path) . dropWhile (not . (==(head path)))  -- for the beginning slash

pathdirname = (\l -> if isEmpty l then "" else last l) . filter (not . isEmpty) . splitOn (=='/')
pathLength = length . splitOn (=='/')

-- |the equivalent of `ls`, list the direct children of a given path in pic-sure
-- lsPath with the empty string switches to lsResources
lsPath :: Bool -> String -> ReaderT Config IO [String]
lsPath _ "" = listServices
lsPath relative path = do
  resp <- picsureGetService' (urlPath </> path)
  let pui = ( f . unString . Utils.Json.lookup "pui")
        where f = if relative then subStrAfterPath path else Prelude.id
  return $ case resp of Nothing -> []
                        Just r -> fmap pui $ r

lsPath' = lsPath False


findPath term = do
  resp <- picsureGetService urlFind [("term", term)]
  return resp

-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs ::
  ([Char] -> ReaderT Config IO [[Char]]) -> ([Char] -> Bool) -> [Char] -> ReaderT Config IO [Char]
bfs nextf checkf startNode = let
  run (node : nodes)
    | checkf node = return node
    | otherwise   = liftIO (putStrLn node) >>
                    nextf node >>= (run . (nodes++))
  in run [startNode]

-- |search a specific node in all the available resources
searchPath :: String -> String -> ReaderT Config IO String
searchPath node from = bfs lsPath' ((==node) . pathdirname) from

-- |search from root
searchPath' node = searchPath node ""

-------- tests --------

completedFile = "data/.completed"

-- |reproduces the data tree in the file system by creating a directory for each item
-- this is for testing purposes, the amount of HTTP requests needed if way to high and
-- this takes ages to complete.
buildPathTree :: [Char] -> ReaderT Config IO ()
buildPathTree fromNode = do
  let go !completed node = do -- bangpattern needed to enforce strictness of reading (file locked error)
        let dirname = "data" </> node
            isNotComplete = not $ elem node completed
        liftIO $ putStrLn ((take (pathLength node - 1) $ repeat ' ') ++ show (pathLength node) ++ (pathdirname node)) -- just being fancy
        when ((pathLength node <= 8) && isNotComplete) $ do
          liftIO $ createDirectoryIfMissing True dirname
          lsPath True node >>= traverse (go completed . (node</>))
          return ()
          -- >> appendFile completedFile (node++"\n")
          -- >> hPutStrLn h (node ++ "\n"))
          -- >>= (return . (node:) . concat))  
  completed <- lines <$> (liftIO $ readFile completedFile)
  go completed fromNode
