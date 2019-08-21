{-# LANGUAGE PartialTypeSignatures #-}
module Folding where

import Control.Lens hiding ((|>))
import qualified Data.Map as M
import Control.Applicative
import Data.Functor.Compose
import Data.Foldable
import Data.Monoid hiding (First)
import Data.Semigroup

dumbExample :: [Maybe [Maybe Int]]
dumbExample = [Just [Just 1, Nothing], Nothing, Just [Just 20, Just 300], Just [Just 4000, Just 50000]]

thingy :: [Maybe [Maybe Int]] -> Int
thingy = sumOf $ traversed . traversed . traversed . traversed

-- betterThingy :: [Maybe [Maybe Int]] -> Int
-- betterThingy = sumOf $ folded . _Just . sequenced . _Just . folded

-- betterThingy :: [Maybe [Maybe Int]] -> Int
-- betterThingy = sumOf $ folded . _Just . sequenced . _Just . folded

betterThingy :: [Maybe [Maybe Int]] -> _
betterThingy = sumOf $ asumming (folded . _Just . sequenced) . folded

foldAListOfMaybes :: [Maybe Int] -> Maybe Int
foldAListOfMaybes s = [s] |> folded |> _Just

type Finalizer s a = [s] -> a
type Selector s a = s -> [a]

type AMonoid r a  = (a, a -> a -> a) -> r -> a
type Transform r r' = forall a. AMonoid r a -> AMonoid r' a

foldMap' :: AMonoid s a -> AMonoid [s] a
foldMap'  ms = \(identity, mappend') xs -> foldr' mappend' identity (fmap (ms (identity, mappend')) xs)



infixl 9 |>
(|>) :: Alternative f => [s] -> Fold s a -> f a
s |> fld = asumOf (folded . fld . to pure) s

-- type Monoidal s a = Fold s a

-- type Monoidal s a = Getting (Maybe (First a), Endo a) s a

-- runMonoid :: Monoidal s a -> s -> a
-- runMonoid mdl s = go
--   where
--     go :: a -> Const (Maybe (First a), Maybe Endo a) a
--     go a = Const (Nothing, Endo id)

asumming :: (Alternative f, Foldable f) => Fold s (f a) -> Fold s a
asumming fld = to (\s -> asum (s ^.. fld )) . folded


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
