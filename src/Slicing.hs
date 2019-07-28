{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Slicing where

import Control.Lens

sliced :: Traversable t => Int -> Int -> Int -> Traversal' (t a) a
sliced start end step = partsOf traversed . sliced' start end step

sliced' :: (Traversable t, Reversing (t a))
       => Int -> Int -> Int
       -> Traversal' (t a) a
sliced' start end stepSize f xs =
    xs
    & partsOf (traversed . indices pickElements)
    . maybeReversed
    . traversed
    . indices stepper
    %%~ f
  where
    size = length xs
    start' = if start >= 0 then start else size + start
    end' = if end >= 0 then end else size + end
    maybeReversed = if stepSize < 0 then reversed else id
    pickElements i = i >= (if stepSize >= 0 then start' else end') && i < (if stepSize >= 0 then end' else start')
    stepper i = i `mod` abs stepSize == 0

isliced :: Traversable t => Int -> Int -> Int -> IndexedTraversal' Int (t a) a
isliced start end step =
    (partsOf (traversed . withIndex) . sliced' start end step . indexBy fst) <. _2
  where
    indexBy :: (a -> i) -> IndexedTraversal' i a a
    indexBy f p a = indexed p (f a) a

-- >>> [1..10] ^.. sliced 5 2 (-1)
-- [5,4,3]
-- >>> [1..10] ^.. sliced 0 5 1
-- [1,2,3,4,5]
-- >>> [1..10] ^.. sliced 0 10 2
-- [1,3,5,7,9]
-- >>> [1..10] ^.. sliced 4 10 2
-- [5,7,9]
-- >>> [1..10] ^.. sliced 10 0 (-2)
-- [10,8,6,4,2]
-- >>> [1..10] ^.. sliced 0 (-3) 1
-- [1,2,3,4,5,6,7]
