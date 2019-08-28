{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Recurse where

import Control.Lens hiding ((:<))
import Data.Function
import Data.Maybe
import Data.Tree
import Data.Tree.Lens
import Control.Comonad.Cofree
import Control.Comonad
import Data.Bifunctor.Join
import Data.Functor.Compose
import Control.Arrow
import Data.Proxy
import Data.Data
import Data.Data.Lens

-- just unfold?
recurseOf :: Traversal' a a -> (a -> a) -> a -> a
recurseOf t f a = f a & t %~ f

odds :: [a] -> [a]
odds = recurseOf _tail (drop 1)

cataOf :: Traversal' a a -> (a -> a) -> a -> a
cataOf t f a = f (a & t %~ cataOf t f)

optimalOfBy :: Traversal' a b -> (b -> b -> Ordering) -> Traversal' a b
optimalOfBy t cmp f s = do
    case maximumByOf (indexing t . withIndex) (cmp `on` snd) s of
        Just (mxi, _) -> (indexing t . index mxi) f s
        Nothing -> ignored f s

optimalOf :: Ord b => Traversal' a b -> Traversal' a b
optimalOf t f s = do
    case maximumByOf (indexing t . withIndex) compare s of
        Just (mxi, _) -> (indexing t . index mxi) f s
        Nothing -> ignored f s



data BT a
    = BT { _leftTree  :: BT a
         , _val   :: a
         , _rightTree :: BT a
         }
    | Leaf
    deriving (Show, Eq, Functor, Foldable, Traversable)
makeLenses ''BT

branchBy :: (a -> Bool) -> Traversal' a b -> Traversal' a b -> Traversal' a b
branchBy p ta tb f s =
    if p s then ta f s
           else tb f s

binSearch :: Int -> BT Int -> Bool
binSearch n = has (deepOf (branchBy (anyOf val (> n)) leftTree rightTree) (val . only n))


--     4
--    / \
--  /     \
-- 2       7
--  \     /
--   3   6
tree :: BT Int
tree =
    BT (BT Leaf 2 (BT Leaf 3 Leaf))
       4
       (BT (BT Leaf 6 Leaf) 7 Leaf)

-- >>> binSearch 2 tree
-- True
-- >>> binSearch 3 tree
-- True
-- >>> binSearch 6 tree
-- True
-- >>> binSearch 20 tree
-- False


type BinTree a = Cofree (Join (,) `Compose` Maybe) a

-- jj :: (Cofree Maybe Int, Cofree Maybe Int)
             -- -> Compose (Join (,)) Maybe (Cofree (Compose (Join (,)) Maybe) Int)
node = curry (Compose . Join)
leaf = Compose (Join (Nothing, Nothing))

bTree :: BinTree Int
bTree = 4 :< node (Just (2 :< node Nothing
                                   (Just (3 :< leaf))))
                  (Just (7 :< node (Just (6 :< leaf))
                                    Nothing))

_L' :: Traversal' (BinTree a) (BinTree a)
_L' = (_unwrap . _Wrapped' . _Wrapped' . _1 . _Just)

_R' :: Traversal' (BinTree a) (BinTree a)
_R' = (_unwrap . _Wrapped' . _Wrapped' . _2 . _Just)

binSearchCofree :: Int -> BinTree Int -> Bool
binSearchCofree n = has (deepOf (branchBy ((>n) . extract) _L' _R') (_extract . only n))


    -- 4
  -- 2   7
   -- 3 6


data KTree f a =
    KTree { _value     :: a
          , _kchildren :: (f (KTree f a))
          }

makeLenses ''KTree

nodes :: forall t a. Traversable t => Traversal' (KTree t a) (KTree Proxy a)
nodes f s =
    let newChildren = (traverse . nodes) f (view kchildren s)
        newTop = f (s & kchildren .~ Proxy)
     in set kchildren <$> newChildren <*> newTop

ktree :: KTree [] Int
ktree =
    KTree 1
          [ KTree 2 [KTree 4 []]
          , KTree 3 []
          ]


