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
import GHC.Generics (Generic)
import qualified Data.Map as M

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

pets :: [(String, [String])]
pets = [ ("Steven", ["Spot", "Mittens"]) , ("Kaylee", ["Pepper", "Sparky"]) ]

 -- >>> pets ^@.. traverse . chooseIndex fst <. _2 . folded
 -- [("Steven","Spot"),("Steven","Mittens"),("Kaylee","Pepper"),("Kaylee","Sparky")]


chooseIndex :: (s -> i) -> IndexedLensLike i f s t s t
chooseIndex getIndex = reindexed getIndex selfIndex

-- >>> pets ^@..   -- Starting with all pets, we want an INDEXED (@) list of focuses
--        folded   -- Fold over the top-list, focusing each (owner, pets) tuple
--     .  reindexed fst selfIndex -- Take the focus as our index, but then apply `fst` to it
--     <. _2  -- <. says to use the earlier index for the next part, then dive into _2
--     . folded -- fold over the list of pet names, focusing each one

-- >>> pets ^@..  folded .  reindexed fst selfIndex <. _2 . folded
-- [ ("Steven", "Spot")
-- , ("Steven", "Mittens")
-- , ("Kaylee", "Pepper")
-- , ("Kaylee", "Sparky")
-- ]

-- `@` always means "with an index", and for `toListOf` a.k.a. `^..` it pairs results up in a tuple!

-- >>> pets ^. folded . to sequence
-- [("Steven","Spot"),("Steven","Mittens"),("Kaylee","Pepper"),("Kaylee","Sparky")]

-- >>> pets ^. folded . to (sequenceOf _2)

-- [("Steven","Spot"),("Steven","Mittens"),("Kaylee","Pepper"),("Kaylee","Sparky")]


-- >>> [(Left 1, "a"), (Right "thing", "b")]
--       ^.. traversed . filteredBy (_1 . _Right) . _2
-- ["b"]

-- >>> [(1, "a"), (2, "b"), (3, "c")]
--       ^.. traversed . filteredBy (_1 . only 2) . _2
-- ["b"]

-- >>> [(M.singleton "secret" "shhhh", "a"), (M.empty, "b")]
--         ^? traversed . filteredBy (_1 . ix "secret") . _2
-- Just "a"

testFilterBy''' = [(M.singleton "secret" "shhhh", "a"), (M.empty, "b")]
    ^? traversed . (filteredBy (_1 . ix "secret") <. _2) . withIndex



