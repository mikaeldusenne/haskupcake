{-# LANGUAGE OverloadedStrings, BangPatterns, LambdaCase, DuplicateRecordFields #-}
module PicSure.Resource where
import Control.Monad.Trans.State

-- import Control.Monad.Plus
import Data.List
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Monad.IO.Class (liftIO)
import PicSure.Utils.Misc
import PicSure.Utils.List
import PicSure.Utils.Trees
import PicSure.Utils.Json
import PicSure.Cache
import PicSure.Utils.Paths
import PicSure.Types
import PicSure.Requester

urlResourceService = "resourceService"
urlResources =       urlResourceService </> "resources"
urlPath =            urlResourceService </> "path"
urlFind =            urlResourceService </> "find"
urlSystemService =   "systemService"
urlAbout =           urlSystemService   </> "about"

about :: PicSureM BSL.ByteString
about = getRequest' urlAbout >>= (return . prettyJson . fromJust .  decodeValue)

-- |list available services on pic-sure
listServices :: PicSureM [String]
listServices = do
  gets cache >>= \case
    (Node _ l@(_:_)) -> return (map treeValue l)
    _ -> do
      l' <- excluded <$> gets config
      liftIO $ print l'
      l  <- filter (not . (`elem`l')) . fmap (unString . PicSure.Utils.Json.lookup "name") . unArray
            <$> listResources
      traverse (modify . (flip addToCache)) l
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
lsPath relative path = perhapsM
                       ((not <$>) . isAbsolute)
                       ((fromJust <$>) . searchPath') path >>=
                       \path -> do
  cachef <- cacheFile <$> gets config
  let l = if cachef==Nothing
          then [path]                            -- if cache is OFF: ["/ab/cd/efgh"]
          else reverse . snd . foldl f ("", [])  -- if cache is ON:  "/ab/cd/efgh" -> ["/","/ab","/ab/cd","/ab/cd/efgh"]
               $ splitPath path
        where f (s, acc) e = (s</>e, s</>e : acc)

      patchDouble e = if basename path == "Demo" then e</>e else e
      go :: [Char] -> PicSureM [String]
      go p = do
        cacheFetch p >>= \case
          (Just (Node _ ll)) -> do -- found in cache
            return . (map (perhaps (not relative) (p</>) . patchDouble . treeValue)) $ ll
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
            f acc e =
              (\case Nothing -> acc
                     e' -> e') $ checkf e
        case (foldl f Nothing (node:nexts)) of
          Nothing -> run $ nodes++nexts
          n -> return $  n
  in run [startNode]

-- |search a specific <node>, starting at the absolute path <from>
searchPath :: String -> String -> PicSureM (Maybe String)
searchPath node from =
  isAbsolute node >>= ((?) (return (Just node)) $ do
  print' ("Searching path", node)
  let (headNode : restNode) = splitPath node
      checkf = boolToMaybe f
        where f p = (headNode == pathbasename p && (length l < 2 || (head . drop 1 . reverse $ l) /= "Demo"))
                where l = splitPath p
  p <- (((\e -> foldl (</>) e restNode)<$>) <$> bfs lsPath' checkf from)
  print' ("found",p)
  case p of Nothing -> return Nothing
            Just p' -> do
              l <- lsPath' (dirname p')
              -- print' ("thelist is", l, p', (any ((==EQ) . comparePaths p') l))
              if (any ((==EQ) . comparePaths p') l) -- (p' `elem` l) -- elem not working because sometimes trailing slashes
                then return p
                else return Nothing
                      )

flattenMaybe (Just (Just e)) = Just e
flattenMaybe _ = Nothing

-- |search in all the available resources
searchPath' :: String -> PicSureM (Maybe String)
searchPath' node = searchPath node "/"

isAbsolute :: String -> PicSureM Bool
isAbsolute "" = return True
isAbsolute "/" = return True
isAbsolute path = any f <$> listServices
  where f service = isPrefixOf ("/"</>service) ("/"</>path) 
