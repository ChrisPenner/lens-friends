{-# LANGUAGE RankNTypes #-}
module Indexing where

import Control.Lens

indexBy :: (a -> i) -> IndexedTraversal' i a a
indexBy f p a = indexed p (f a) a
