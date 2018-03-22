module PicSure.Utils.Trees where

import Data.List(foldl')

data Tree α = Node α [Tree α] | Leaf α | Empty

instance (Show α) => Show (Tree α) where
  show = unlines . sh ""
    where sh s (Node a l) = leaf s a
                            : concatMap (sh (s++tab)) l
          sh s (Leaf a) = [leaf s a]
          tab="  "
          leaf s a = s ++ ">" ++ show a
            

instance (Read α) => Read (Tree α) where
  readsPrec k r = [(Node a l, s''') |
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

instance (Eq α) => Monoid (Tree α) where
  mempty = Empty
  mappend (Node a l) n = Node a $ add l n
    where add :: (Eq α) => [Tree α] -> Tree α -> [Tree α]
          add [] t = [t]
          add (a:as) b
            | treeValue a == treeValue b = case a of
                (Node na la) -> case b of
                  (Node nb lb) -> Node na (foldl add la lb) : as
                  (Leaf nb)    -> a : as
                (Leaf na)    -> b : as
            | otherwise = a : add as b
          --       | otherwise = a : add as b
          -- add (nodea@:xs) node@(Node n l)
          --   | na == n = 
          --   | otherwise = nodea : add xs node
          -- add (nodea@(Leaf na):xs) node@(Node n l)
          --   | na == n = node : xs
          --   | otherwise = nodea : add xs node
          -- all (nodea@(Node na la):xs) node@(Node n l)

treeValue (Leaf a) = a
treeValue (Node a l) = a
                                  
toList :: Tree a -> [a]
toList = foldl' (\acc e -> acc++[e]) []

toListLeafs (Node a l) = foldl' (++) [] . map toListLeafs $ l
toListLeafs (Leaf a) = [a]

fromList :: [a] -> Tree a
fromList [x] = Leaf x
fromList (x:xs) = Node x [fromList xs]

-- buildTree :: (α -> [α]) -> α -> Tree α
-- buildTree f a = Node a . map buildTree $ f a

data BTree a = BLeaf a | BNode (BTree a) (BTree a)
  deriving Show 


instance (Read a) => Read (BTree a) where
  readsPrec d r' = readParen (d > app_prec)
                  (\r -> [(BLeaf m,t) |
                           ("BLeaf",s) <- lex r,
                           (m,t) <- readsPrec (app_prec+1) s]) r'
                  ++ readParen (d > up_prec)
                  (\r -> [(BNode u v,w) |
                           (u,s) <- readsPrec (up_prec+1) r,
                           (":^:",t) <- lex s,
                           (v,w) <- readsPrec (up_prec+1) t]) r'
    where app_prec = 10
          up_prec = 5
