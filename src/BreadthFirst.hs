{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}
module BreadthFirst where

import Control.Lens hiding ((:<))
import Data.Tree
import Control.Monad.State
import Data.IntMap
import Data.Function
import Data.Tree.Lens
import Control.Applicative
import Control.Arrow
import Control.Comonad.Cofree
import Control.Lens.Unsound

data Ap w f a where
  Pure :: a -> Ap w f a
  Ap   :: w -> f a -> Ap w f (a -> b) -> Ap w f b

instance Functor (Ap w f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Ap w x y)   = Ap w x ((f .) <$> y)

instance (Ord w) => Applicative (Ap w f) where
  pure = Pure
  Pure f <*> y = fmap f y
  a@(Ap w x y) <*> b
    | getPriority a <= getPriority b = Ap w x (flip <$> y <*> b)
    | otherwise = b <**> a

liftAp :: w -> f a -> Ap w f a
liftAp w fa = Ap w fa (Pure id)

-- | This must be a monotonic (i.e. order preserving) function
--
-- That is, @a >= b implies f a >= f b@
mapPriority :: (w -> x) -> Ap w f a -> Ap x f a
mapPriority _ (Pure a) = Pure a
mapPriority f (Ap w x next) = Ap (f w) x (mapPriority f next)

getPriority :: Ap w f x -> Maybe w
getPriority (Pure _) = Nothing
getPriority (Ap w _ _) = Just w

priority :: w -> f a -> Ap w f a
priority w fa = Ap w fa (Pure id)

retract :: Applicative f => Ap w f a -> f a
retract (Pure a) = pure a
retract (Ap _ fx next) = fx <**> retract next

action :: IO ()
action = retract $
    priority 3 (print "three")
    *> priority 5 (print "five")
    *> priority 1 (print "one")
    *> (priority 10 (print "ten")
        *> priority 4 (print "four")
        *> priority 6 (print "six"))
    *> priority 0 (print "zero")

breadthFirst :: Traversable f => Traversal (Cofree f a) (Cofree f b) a b
breadthFirst f s = retract $ breadthFirst' f s

breadthFirst' :: Traversable g => (a -> f b) -> Cofree g a -> Ap (Int, [Int]) f (Cofree g b)
breadthFirst' f (a :< c) =
    (:<) <$> priority (0, []) (f a)
    <*> (itraverseOf traversed nextLayer c)
  where
    nextLayer i x = mapPriority (succ *** (<> [i])) $ breadthFirst' f x

treeBreadthFirst :: Traversal (Tree a) (Tree b) a b
treeBreadthFirst f s = retract $ breadthFirstTree f s

breadthFirstTree :: (a -> f b) -> Tree a -> Ap (Int, [Int]) f (Tree b)
breadthFirstTree f (Node a c) =
    Node <$> priority (0, []) (f a)
    <*> (itraverse nextLayer c)
  where
    nextLayer i x = mapPriority (succ *** (<> [i])) $ breadthFirstTree f x

tree :: Tree Int
tree = Node 1 [Node 2 [Node 4 [Node 5 [Node 6 [Node 7 []]]]], Node 3 [Node 40 [], Node 50 []]]

cotree :: Cofree [] Int
cotree = 1 :< [2 :< [4 :< [5 :< [6 :< [7 :< []]]]], 3 :< [40 :< [], 50 :< []]]

infinite :: IO ()
infinite = retract $ go 0
    where
      go n = priority n (print n) *> go (n + 1)

breadth :: Plated s => Traversal' s a -> Traversal' s a
breadth = breadthOf plate

breadthOf :: Traversal' s s -> Traversal' s a -> Traversal' s a
breadthOf recursions t f s = retract (s & breadthOf' recursions t %%~ liftAp (0, []) . f)

breadthOf' :: forall f s a. Traversal' s s -> Traversal' s a -> LensLike' (Ap (Int, [Int]) f) s a
breadthOf' recursions t = splitParts . findFocuses
    where
      increaseDepth :: Ap (Int, d) f x -> Ap (Int, d) f x
      increaseDepth = mapPriority (first succ)
      splitParts :: Lens' s ([s], [a])
      splitParts = lensProduct (partsOf recursions) (partsOf t)
      findFocuses :: LensLike' (Ap (Int, [Int]) f) ([s], [a]) a
      findFocuses f s =
          increaseDepth (s & beside (traversed . tagLocations . breadthOf' recursions t) traversed %%~ f)

      tagLocations :: (s -> Ap (x, [Int]) f t) -> Indexed Int s (Ap (x, [Int]) f t)
      tagLocations f = Indexed $ \i s -> mapPriority (second (<> [i])) (f s)
