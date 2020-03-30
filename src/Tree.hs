module Tree where

-- https://www.geeksforgeeks.org/tree-traversals-inorder-preorder-and-postorder/

import Control.Lens

data BinTree a = Leaf | Branch (BinTree a) a (BinTree a)
  deriving (Show)

inorder :: Traversal (BinTree a) (BinTree b) a b
inorder _ Leaf = pure Leaf
inorder f (Branch l a r) = Branch <$> inorder f l <*> f a <*> inorder f r

preorder :: Traversal (BinTree a) (BinTree b) a b
preorder _ Leaf = pure Leaf
preorder f (Branch l a r) = (\a' l' r' -> Branch l' a' r') <$> f a <*> preorder f l <*> preorder f r

postorder :: Traversal (BinTree a) (BinTree b) a b
postorder _ Leaf = pure Leaf
postorder f (Branch l a r) = (\l' r' a' -> Branch l' a' r') <$> postorder f l <*> postorder f r <*> f a

t :: BinTree Int
t = Branch (Branch (Branch Leaf 4 Leaf) 2 (Branch Leaf 5 Leaf)) 1 (Branch Leaf 3 Leaf)
