{-# LANGUAGE ScopedTypeVariables #-}
module Sorting where

import Control.Lens
import Data.List
import Data.Function

sortedByOf :: (a -> a -> Ordering) -> Traversal' s a -> Traversal' s a
sortedByOf cmp t = partsOf t . sortedOf cmp

sortedOnOf :: Ord k => (a -> k) -> Traversal' s a -> Traversal' s a
sortedOnOf cmp t = partsOf t . sortedOf (compare `on` cmp)

sortedOf :: forall a. (a -> a -> Ordering) -> Traversal' [a] a
sortedOf cmp f as =
    let ordered :: [(Int, a)] = zip ([0..]) as
        sorted :: [(Int, a)] = sortBy (cmp `on` snd) ordered
        altered = (traverse . _2) f sorted
        reordered = fmap snd . sortOn fst <$> altered
     in reordered
