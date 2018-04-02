{-# LANGUAGE OverloadedStrings, BangPatterns, LambdaCase, DuplicateRecordFields #-}
module PicSure.Resource where
import Control.Monad.Trans.State

import Control.Monad.Plus
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
          (Just (Node _ ll)) -> do -- found in cache
            return . (map (perhaps (not relative) (p</>) . treeValue)) $ ll
          _ -> do -- not in cache: perform request
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

-- |custom breadth first search
-- assumes that the tree is acyclic (which it is with pic-sure)
-- and that the nextf function returns a list of absolute paths
bfs :: (String -> PicSureM [String])
    -> (String -> Maybe String)
    -> String
    -> PicSureM (Maybe String)
bfs nextf checkf startNode = let
  run :: [String] -> PicSureM (Maybe String)
  run [] = return Nothing  -- found nothing
  run (node : nodes) = do
        -- liftIO $ putStrLn node
        nexts <- nextf node
        let f :: Maybe String -> String -> Maybe String
            -- f acc e = acc `mplus` checkf e
            f acc e = acc `mplus` checkf e
        case (foldl f Nothing (node:nexts)) of
          Nothing -> run $ nodes++nexts
          n -> return $ (node</>) <$> n
  in run [startNode]

-- |search a specific <node>, starting at the absolute path <from>
searchPath :: String -> String -> PicSureM (Maybe String)
searchPath node from = 
  isAbsolute node >>= ((?) (return (Just node)) $ do
  let (headNode : restNode) = splitPath node
      checkf = boolToMaybe f
        where f p = (headNode == pathbasename p && (length l < 2 || (head . drop 1 $ l) /= "Demo"))
                where l = splitPath p
  pp <- bfs lsPath' checkf from
  let p = ((\e -> foldl (</>) e restNode)<$>) pp
  case p of Nothing -> return Nothing
            Just p' -> do
              print' ("~~~~~~~~~~~~~~~~~", pp)
              print' ("~~~~~~~~~~~~~~~~~", p')
              
              lsPath' (dirname p') >>= \l -> return $
                                   if (p' `elem` l)
                                   then p
                                   else Nothing
                      )

flattenMaybe (Just (Just e)) = Just e
flattenMaybe _ = Nothing

-- |search in all the available resources
searchPath' :: String -> PicSureM (Maybe String)
searchPath' node = searchPath node "/"

isAbsolute :: String -> PicSureM Bool
isAbsolute path = any f <$> listServices
  where f service = isPrefixOf ("/"</>service) ("/"</>path) 
