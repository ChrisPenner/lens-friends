{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Sorting where

import Control.Lens
import Data.List
import Data.Function
import Data.Tree hiding ( levels )
import Data.Tree.Lens
import qualified Data.Map as M
import Control.Monad
import Data.Functor.Compose

sortedOf :: Ord a => Traversal' s a -> Traversal' s a
sortedOf t = sortedOnOf id t

sortedByOf :: (a -> a -> Ordering) -> Traversal' s a -> Traversal' s a
sortedByOf cmp t = partsOf t . sortedListByOf cmp

sortedOnOf :: Ord k => (a -> k) -> Traversal' s a -> Traversal' s a
sortedOnOf cmp t = partsOf t . sortedListByOf (compare `on` cmp)

unsafeSortedOf :: Ord a => Traversal s t a b -> Traversal s t a b
unsafeSortedOf t = unsafeSortedOnOf id t

unsafeSortedByOf :: (a -> a -> Ordering) -> Traversal s t a b -> Traversal s t a b
unsafeSortedByOf cmp t = unsafePartsOf t . sortedListByOf cmp

unsafeSortedOnOf :: Ord k => (a -> k) -> Traversal s t a b -> Traversal s t a b
unsafeSortedOnOf cmp t = unsafePartsOf t . sortedListByOf (compare `on` cmp)

sortedListByOf :: forall a b. (a -> a -> Ordering) -> Traversal [a] [b] a b
sortedListByOf cmp f as =
    let ordered :: [(Int, a)] = zip ([0..]) as
        sorted :: [(Int, a)] = sortBy (cmp `on` snd) ordered
        altered = (traverse . _2) f sorted
        reordered = fmap snd . sortOn fst <$> altered
     in reordered

prompt :: String -> IO String
prompt s = do
    putStrLn (s <> ":")
    putStr "> "
    getLine

-- fromList [("bar",1),("baz",6),("foo",16)]
-- >>> [(2, "two"), (3, "three"), (1, "one")] & sortedOnOf fst traverse . _2 %%~ prompt
-- one:
-- > first
-- two:
-- > second
-- three:
-- > third
-- [(2,"second"),(3,"third"),(1,"first")]


runLookups :: Ord k => M.Map k Int -> [[k]] -> [[Int]]
runLookups m = (traversed . traversed) %~ \k -> M.findWithDefault 0 k m

runLookups'' :: Ord k => [[k]] -> (M.Map k Int -> [[Int]])
runLookups'' = (traversed . traversed) %%~ M.findWithDefault 0

runLookups''' :: Ord k => [[k]] -> (M.Map k Int -> [[Int]])
runLookups''' = fmap getCompose . traverse (M.findWithDefault 0) . Compose

exMap :: M.Map String Int
exMap = M.fromList [("foo", 10), ("bar", 1), ("baz", 5)]
-- >>> scanl1Of (sortedOf traverse) (+) exMap


-- 1
-- 2 3
-- 4 40 50
-- 5

tree :: Tree Int
tree = Node 1 [Node 2 [Node 4 [Node 5 [Node 6 [Node 7 [tree]]]]], Node 3 [Node 40 [], Node 50 []]]

-- withDepth :: Int -> Traversal' s s -> Traversal' s a -> Traversal' s (Int, a)
-- withDepth n nextLayers elements f s =
--     let thisLayer = s & elements %%~ \a -> fmap snd (f (n, a))
--      in undefined

-- sortedByOf :: (a -> a -> Ordering) -> Optic' p f s a -> Optic' p f s a
-- sortedByOf cmp t = partsOf t . sortedOf cmp

-- sortedOnOf :: Ord k => Applicative f => (a -> k) -> Traversing' (->) f s a -> LensLike' f s a
-- sortedOnOf cmp t = partsOf t . sortedOf (compare `on` cmp)

-- sortedOf :: forall a p f. (Conjoined p, Applicative f) => (a -> a -> Ordering) ->  Optic' p f [a] a
-- sortedOf cmp = cloneIndexPreservingTraversal (sortedOf' cmp)

-- sortedOf' :: forall a p f. Applicative f => (a -> a -> Ordering) ->  LensLike' f [a] a
-- sortedOf' cmp f as =
--     let ordered :: [(Int, a)] = zip ([0..]) as
--         sorted :: [(Int, a)] = sortBy (cmp `on` snd) ordered
--         altered :: f [(Int, a)]= sorted & traverse %%~ \(i, y) -> (i,) <$> f y
--         reordered = fmap snd . sortOn fst <$> altered
--      in reordered

-- sortedByIndex :: Optic p f s t a b -> Optic p f s t a b
-- sortedByIndex t = sortedOnOf fst (t . withIndex) . selfIndex . _2
