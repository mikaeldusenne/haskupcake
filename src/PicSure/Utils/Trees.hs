{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module PicSure.Utils.Trees where

import Data.List
import GHC.Generics (Generic)

import qualified Data.Binary as B

import Data.List(foldl')
import Control.Monad.Plus

data Tree α = Node α [Tree α] | Leaf α | Empty
  deriving(Generic, Eq)

instance B.Binary a => B.Binary (Tree a)

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
                     (Leaf _) -> nb
                     (Node a l) -> case nb of (Leaf _) -> nb
                                              (Node _ l') -> Node a $ foldl add l l'
    where add [] t = [t]
          add (a:as) b
            | treeValue a == treeValue b = mappend a b : as
            | otherwise = a : add as b

-- addAfter t l t'

treeValue Empty = error "called treeValue on Empty Tree"
treeValue (Leaf a) = a
treeValue (Node a _) = a

treeChildren (Node _ l) = Just l
treeChildren _          = Nothing


toList :: Tree a -> [a]
toList = foldl' (\acc e -> acc++[e]) []

fromList :: [a] -> Tree a
fromList [] = Empty
fromList [x] = Leaf x
fromList (x:xs) = Node x [fromList xs]

fromList' :: [a] -> [Tree a] -> Tree a
fromList' [] _ = Empty
fromList' [x] l = Node x l
fromList' (x:xs) l = Node x [fromList' xs l]

treeFind :: Eq a => Tree a -> [a] -> Maybe (Tree a)
treeFind Empty _    = Nothing
treeFind l@(Leaf a) [x] = if a == x then Just l else Nothing
treeFind (Leaf _) _ = Nothing
treeFind (Node _ _) [] = Nothing
treeFind node@(Node n _) [x] | n == x = Just node
                             | otherwise = Nothing
treeFind (Node n l) (x:xs)
  | n == x = foldl mplus Nothing (map (`treeFind`xs) l)
      -- (\case Nothing -> Nothing
      --        Just e -> e) . safe_head . filter (/=Nothing) $ map (`treeFind`xs) l
  | otherwise = Nothing
