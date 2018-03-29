{-# LANGUAGE DeriveGeneric #-}
module PicSure.Utils.Trees where

import Data.List
import GHC.Generics (Generic)

import qualified Data.Binary as B

import Data.List(foldl')
import Control.Monad.Plus
import PicSure.Utils.List
import PicSure.Utils.Misc

data Tree α = Node α [Tree α] | Leaf α | Empty
  deriving(Generic)

instance B.Binary a => B.Binary (Tree a)

-- instance (Show α) => Show (Tree α) where
--   show = unlines . sh ""
--     where sh s (Node a l) = leaf s a
--                             : concatMap (sh (s++tab)) l
--           sh s (Leaf a) = [leaf s a]
--           sh s Empty    = ["ø"]
--           tab="  "
--           leaf s a = s ++ "." ++ show a

instance (Show α) => Show (Tree α) where
  show (Node a l) = "(" ++ show a ++ ",[" ++ (intercalate "," $ map show l) ++ "])"
  show (Leaf a) = "(" ++ show a ++ ")"
  show Empty = "(ø)"

instance (Read α) => Read (Tree α) where
  readsPrec k r | isInfixOf "(ø)" r = [(Empty, drop 3 r)]
                | otherwise = [(Node a l, s''') |
                   ("(",s)   <- lex r,
                   (a,',':s') <- readsPrec (k+1) s,
                   (l, s'')   <- readsPrec (k+1) s',
                   (")",s''') <- lex s'']
                  ++ [(Leaf a,s') |
                      ("(",s) <- lex r,
                      (a, ')':s') <- readsPrec (k+1) s]


instance Functor Tree where
  fmap f (Node a l) = Node (f a) $ map (fmap f) l
  fmap f (Leaf a)   = Leaf (f a)

instance Foldable Tree where
  foldMap f (Node a l) = f a `mappend` foldMap (foldMap f) l
  foldMap f (Leaf a)   = f a
  
instance Traversable Tree where
  traverse f (Node a l) = Node <$> f a <*> traverse (traverse f) l
  traverse f (Leaf a)   = Leaf <$> f a

instance (Eq α, Show α) => Monoid (Tree α) where
  mempty = Empty
  mappend Empty e = e
  mappend e Empty = e
  mappend na nb  | treeValue na /= treeValue nb = error . unlines $ ["could not add ", show na, "to", show nb]
                 | otherwise = case na of
                     (Leaf a) -> nb
                     (Node a l) -> case nb of (Leaf _) -> nb
                                              (Node _ l') -> Node a $ foldl add l l'
    where add [] t = [t]
          add (a:as) b
            | treeValue a == treeValue b = mappend a b : as
            | otherwise = a : add as b



-- instance (Eq α) => Monoid (Tree α) where
--   mempty = Empty
--   mappend (Leaf a) n = Node a [n]
--   mappend Empty e = e
--   mappend e Empty = e
--   mappend (Node a l) n = Node a $ add l n
--     where add :: (Eq α) => [Tree α] -> Tree α -> [Tree α]
--           add [] t = [t]
--           add (a:as) b
--             | treeValue a == treeValue b = case a of
--                 (Node na la) -> case b of
--                   (Node nb lb) -> Node na (foldl add la lb) : as
--                   (Leaf nb)    -> a : as
--                 (Leaf na)    -> b : as
--             | otherwise = a : add as b
--           --       | otherwise = a : add as b
--           -- add (nodea@:xs) node@(Node n l)
--           --   | na == n = 
--           --   | otherwise = nodea : add xs node
--           -- add (nodea@(Leaf na):xs) node@(Node n l)
--           --   | na == n = node : xs
--           --   | otherwise = nodea : add xs node
--           -- all (nodea@(Node na la):xs) node@(Node n l)

treeValue (Leaf a) = a
treeValue (Node a l) = a

treeChildren (Node _ l) = Just l
treeChildren _          = Nothing


toList :: Tree a -> [a]
toList = foldl' (\acc e -> acc++[e]) []

toListLeafs (Node a l) = foldl' (++) [] . map toListLeafs $ l
toListLeafs (Leaf a) = [a]

fromList :: [a] -> Tree a
fromList [] = Empty
fromList [x] = Leaf x
fromList (x:xs) = Node x [fromList xs]

treeFind :: Eq a => Tree a -> [a] -> Maybe (Tree a)
treeFind Empty _    = Nothing
treeFind l@(Leaf a) [x] = if a == x then Just l else Nothing
treeFind l@(Leaf a) _ = Nothing
treeFind (Node _ l) [] = Nothing
treeFind node@(Node n l) [x] | n == x = Just node
                             | otherwise = Nothing
treeFind (Node n l) (x:xs)
  | n == x = foldl mplus Nothing (map (`treeFind`xs) l)
  | otherwise = Nothing

-- -- buildTree :: (α -> [α]) -> α -> Tree α
-- -- buildTree f a = Node a . map buildTree $ f a

-- data BTree a = BLeaf a | BNode (BTree a) (BTree a)
--   deriving Show 


-- instance (Read a) => Read (BTree a) where
--   readsPrec d r' = readParen (d > app_prec)
--                   (\r -> [(BLeaf m,t) |
--                            ("BLeaf",s) <- lex r,
--                            (m,t) <- readsPrec (app_prec+1) s]) r'
--                   ++ readParen (d > up_prec)
--                   (\r -> [(BNode u v,w) |
--                            (u,s) <- readsPrec (up_prec+1) r,
--                            (":^:",t) <- lex s,
--                            (v,w) <- readsPrec (up_prec+1) t]) r'
--     where app_prec = 10
--           up_prec = 5
