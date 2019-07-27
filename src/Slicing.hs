{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Slicing where

import Control.Lens
import Control.Applicative

sliceT :: (Traversable t, Applicative f, Reversing (t a))
       => Int -> Int -> Int
       -> (a -> f a)
       -> t a
       -> f (t a)
sliceT start end stepSize f xs =
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
