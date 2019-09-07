{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Scrap where

import GHC.TypeLits
import Data.Kind
import Control.Lens
import Control.Lens.Plated
import Data.Word
import Data.Bits.Lens
import Data.Bifunctor
import Data.Data.Lens
import Control.Applicative
import Data.Function
import Data.Foldable
import Data.Monoid
import GHC.Generics hiding (S)

data SymbolOrType where
  S :: Symbol -> SymbolOrType
  T :: Type -> SymbolOrType

type family MyFamily (x :: SymbolOrType) where
  MyFamily (S s) = "got symbol"
  MyFamily (T t) = "got type"

data Blah = Blah Int Int

makeLenses ''Blah

structure :: (forall a. g a -> f (g a)) -> g x -> f (g x)
structure = id

test :: String -> Word8 -> Bool
test s n = map (=='1') s == n ^.. bits

cosmosOf :: forall a. Traversal' a a -> Traversal' a a
cosmosOf d = (go d)
  where
    go :: forall f. Applicative f => ((a -> f a) -> a -> f a) -> (a -> f a) -> a -> f a
    go = fix (\r d f s -> f s *> d (r d f) s)


instance (Semigroup a) => Semigroup (ZipList a) where
  (<>) = liftA2 (<>)

-- fold :: forall t m. (Foldable t, Monoid m) => t m -> m
-- fold @_ @(via (Sum Int)) ([1..10] :: [Int])

-- ($) :: forall a b. (a -> b) -> a -> b
-- ($) f = (f <$>) @(via Identity)

-- Identity :: a -> Identity a

-- (<$>) :: forall f a b. Functor f => (a -> b) -> f a -> f b
-- (<$>) :: forall f a b. f a -> f b

-- (<>) :: Semigroup a => a -> a -> a


-- foldr' :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- foldr' f b ta = fold @(via (fmap (Dual . Endo))) (f <$> ta) $ b



-- Endo :: (a -> a) -> Endo a
-- >>> :t Dual
-- Dual :: a -> Dual a

-- (Dual . Endo) :: (a -> a) -> Dual (Endo a)
-- Dual (Endo a) -> (a -> a)


-- [1, 2, 3] & (ix 40 `failing` ix 30 `failing` ix 1) .~ 20

ex' :: [Int]
ex' = [1, 2, 3] & fold [ix 40, ix 30, ix 1] .~ 20

-- if it isn't

-- if
-- then
-- else
-- error
-- it
-- isn't
-- has
-- hasn't
-- pure
-- return
-- or
-- and
-- True
-- False
-- do

-- -- do or; do not. if _Nothing `isn't` Just Right then _ else _


