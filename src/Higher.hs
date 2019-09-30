module Higher where

import Data.Coerce

import Control.Lens

data StackF a
  = Push Int a
  | Top (Int -> a)
  | Pop a
  | Add a
  deriving Functor

makePrisms ''StackF

type HPrism f g f' g' = forall a. Prism (f a) (g a) (f' a) (g' a)
