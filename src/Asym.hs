{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Asym where

import Control.Lens
import Control.Applicative

sliceT ::
  (Traversable t, Applicative f) =>
  Int -> Int -> Int -> (a -> f a) -> t a -> f (t a)
sliceT start end stepSize f xs = xs & traversed . indices checkIndex %%~ f
  where
    size = length xs
    start' = if start >= 0 then start else size + start
    end' = if end >= 0 then end else size + end
    checkIndex i = i >= start' && i < end' && ((i - start') `mod` stepSize == 0)
