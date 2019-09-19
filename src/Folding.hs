-- {-# LANGUAGE PartialTypeSignatures #-}
module Folding where

import Control.Lens hiding ((|>))
import qualified Data.Map as M
import Control.Applicative
import Data.Functor.Compose
import Data.Foldable
import Data.Monoid hiding (First)
import Data.Semigroup
import Control.Arrow
import Data.Functor.Contravariant

dumbExample :: [Maybe [Maybe Int]]
dumbExample = [Just [Just 1, Nothing], Nothing, Just [Just 20, Just 300], Just [Just 4000, Just 50000]]

thingy :: [Maybe [Maybe Int]] -> Int
thingy = sumOf $ traversed . traversed . traversed . traversed

-- betterThingy :: [Maybe [Maybe Int]] -> Int
-- betterThingy = sumOf $ folded . _Just . sequenced . _Just . folded

-- betterThingy :: [Maybe [Maybe Int]] -> Int
-- betterThingy = sumOf $ folded . _Just . sequenced . _Just . folded

betterThingy :: [Maybe [Maybe Int]] -> Int
-- betterThingy = sumOf $ asumming (folded . _Just . sequenced) . folded
betterThingy = sum' ~<> asumming (folded . _Just . sequenced) . folded

other :: Int
other = dumbExample & sum' ~<> asumming (folded . _Just . sequenced) . folded

type AMonoid' m = (m, m -> m -> m)
sum' :: Num n => AMonoid' n
sum' = (0, (+))


infixl 8 ~<>
(~<>) :: (m, m -> m -> m) -> Fold s m -> s -> m
(id', mappend') ~<> fld = foldByOf fld mappend' id'

-- foldAListOfMaybes :: [Maybe Int] -> Maybe Int
-- foldAListOfMaybes s = [s] |> folded |> _Just

type Finalizer s a = [s] -> a
type Selector s a = s -> [a]

type AMonoid s r  = (r, r -> r -> r) -> s -> r
type Transform r r' = forall a. AMonoid r a -> AMonoid r' a

foldMap' :: AMonoid s a -> AMonoid [s] a
foldMap' = lift folded

-- runAThing :: () -> AMonoid [s] s
-- runAThing = foldMap' . monoid

lift :: forall s a r. Fold s a -> AMonoid a r -> AMonoid s r
lift fld ms (identityR, mappendR) s =
    let toResult :: a -> r = ms (identityR, mappendR)
     in foldByOf (fld . to toResult) mappendR identityR s


inJust :: AMonoid a r -> AMonoid (Maybe a) r
inJust = lift _Just

monoid :: Monoid m => a -> AMonoid m m
monoid = undefined

-- AAMonoid s r = LensLike' ((->) r) a b

x'd :: AMonoid s r -> AMonoid [Maybe s] r
-- x'd :: [Maybe s] -> r
x'd = foldMap' . inJust

-- q :: AMonoid s a -> s -> a
-- q = foldMap' . foldMap' . _




-- infixl 9 |>
-- (|>) :: (AMonoid w a2 -> AMonoid s a2) -> AMonoid w a2 -> AMonoid t a2
-- m1 |> m2 = _


-- infixl 9 |>
-- (|>) :: Alternative f => [s] -> Fold s a -> f a
-- s |> fld = asumOf (folded . fld . to pure) s

-- type Monoidal s a = Fold s a

-- type Monoidal s a = Getting (Maybe (First a), Endo a) s a

-- runMonoid :: Monoidal s a -> s -> a
-- runMonoid mdl s = go
--   where
--     go :: a -> Const (Maybe (First a), Maybe Endo a) a
--     go a = Const (Nothing, Endo id)

asumming :: (Alternative f, Foldable f) => Fold s (f a) -> Fold s a
asumming fld = to (\s -> asum (s ^.. fld )) . folded

sequencing :: (Applicative f) => Fold s (f a) -> Fold s (f [a])
sequencing fld = to (\s -> sequenceA (s ^.. fld))

inside' :: (Functor f) => Getter s a -> Fold (f s) (f a)
inside' l f s = phantom $ f (fmap (view l) s)

altOf :: forall s m f. (Alternative f, Foldable f) => Fold s (f m) -> Fold s m
altOf fld = to (^.. fld) . to asum . folded
  where
    go :: s -> f m
    go s = asumOf fld s


bettererThingy :: [Maybe [Maybe Int]] -> Int
bettererThingy = sumOf $ folded . _Just . altOf sequenced . folded

sequenced :: (Traversable t, Applicative f) => Fold (t (f a)) (f (t a))
sequenced = to sequenceA

counter :: Ord a => Fold s a -> s -> M.Map a Int
counter f = foldMapByOf f (M.unionWith (+)) mempty (flip M.singleton 1)

aggregateOf :: (Ord k, Monoid m)
            => Fold s (k, m)
            -> s
            -> M.Map k m
aggregateOf fld = foldMapByOf fld (M.unionWith mappend) mempty (uncurry M.singleton)

aggregateByOf :: (Monoid m, Ord k)
              => Fold s a
              -> (a -> k)
              -> (a -> m)
              -> s
              -> M.Map k m
aggregateByOf fld f g = aggregateOf (fld . to (f &&& g))

-- >>> aggregateOf (folded . both . to (length &&& (:[]))) animals
-- fromList [(3,["cat","dog","ant"]),(5,["snake"]),(6,["rabbit"]),(9,["albatross"])]
-- >>> aggregateByOf (folded . both) length (:[]) animals
-- fromList [(3,["cat","dog","ant"]),(5,["snake"]),(6,["rabbit"]),(9,["albatross"])]

-- >>> let x = [[Right 1], [Right 10, Left "ERROR"]]
-- >>> sumOf (asumming (folded . traversed)) x
-- 1

foldMapM :: (Foldable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f s = getAp $ foldMap (Ap . f) s

foldMapIO :: (Foldable t, Monoid b) => (a -> IO b) -> t a -> IO b
foldMapIO = foldMap

-- foldMapM :: (Monad m, Monoid b, Foldable t) => (a -> m b) -> t a -> m b
-- foldMapM k = foldM (\b a -> mappend b <$> k a) mempty
