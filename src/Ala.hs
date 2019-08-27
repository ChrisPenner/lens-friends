module Ala where

import Control.Lens
import Data.Monoid
import Data.Bifunctor.Join
import Data.Bifunctor.Tannen
import Data.Profunctor
import Data.Functor.Compose

doThing :: [[[Int]]]
doThing = alaf Compose traverse (:[]) [[1, 2, 3] :: [Int], [4, 5, 6]]

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- use uncurrying
-- on :: ((b, b) -> c) -> (a -> b) -> (a, a) -> c
-- use (a, a) ~~ Join (,) a
-- on :: (Join (,) b -> c) -> (a -> b) -> Join (,) a -> c
-- use (f a -> b) ~~ Costar f a b
-- on :: Costar (Join (,)) b c -> (a -> b) -> Costar (Join (,)) a c
-- re-associate arguments via (curry . swap . uncurry)
on :: (a -> b) -> Costar (Join (,)) b c -> Costar (Join (,)) a c
on = lmap

-- liftA2 :: (e -> a) -> (e -> b) -> (a -> b -> c) -> (e -> c)
-- liftA2 :: ((e -> a),  (e -> b)) -> ((a, b) -> c) -> (e -> c)
liftA2 :: (Tannen ((->) e) (,)  a b) -> ((,) a b -> c) -> (e -> c)
-- liftA2 :: (Biff (,) ((->) e) ((->) e) a b) -> ((,) a b -> c) -> (e -> c)
-- liftA2 :: ((e -> a), (e -> b), ((a, b) -> c)) -> (e -> c)
liftA2 = undefined
