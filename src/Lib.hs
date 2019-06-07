{-# LANGUAGE RankNTypes #-}
module Lib where

import Control.Lens

experimenting :: Functor f => (s -> f s) -> Getter s a -> Getter s (f a)
experimenting mapper getter = to go
  where
    go s = view getter <$> mapper s
