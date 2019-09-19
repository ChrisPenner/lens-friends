{-# LANGUAGE TypeFamilies #-}
module Grate where

import Control.Lens as L
import Lens.Family2 hiding (zipWithOf)
import Lens.Family2.Unchecked
import Data.Function as F
import Data.Maybe
import Control.Comonad
import Data.Distributive
import Data.Functor.Rep as Rep
-- type GrateLike (g :: Type -> Type) s t a b = (g a -> b) -> g s -> t
-- type GrateLike (g :: Type -> Type) s t a b = (g a -> b) -> g s -> t
-- type Grate g s t a b = (g a -> b) -> g s -> t

zipWithOf :: forall s t a b. Grate s t a b -> (a -> a -> b) -> s -> s -> t
zipWithOf g f s1 s2 = degrated $ \idx -> (f `F.on` idx) s1 s2
    where
      degrated :: ((s -> a) -> b) -> t
      degrated = degrating g

func :: Grate (r -> a) (r -> b) a b
func = grate go
  where
    go :: (((r -> a) -> a) -> b) -> r -> b
    go indexer r = indexer ($ r)


-- | A grate over lists which cycles once past defined data
fixedSize :: Int -> Grate [a] [b] a b
fixedSize n = grate (go 0)
  where
    go :: Int -> (([a] -> a) -> b) -> [b]
    go n' f | n == n' = []
    go n' f =  f ((^?! ix n') . cycle) : go (n' + 1) f

cyclize :: Grate [a] [b] a b
cyclize = grate (go 0)
  where
    go :: Int -> (([a] -> a) -> b) -> [b]
    go n f =  f ((^?! ix n) . cycle) : go (n + 1) f

-- | A grate over infinite lists which pads once past defined data.
paddize :: forall a b. a -> Grate [a] [b] a b
paddize a = grate (go 0)
  where
    go :: Int -> (([a] -> a) -> b) -> [b]
    go n f = f (fromMaybe a . L.preview (ix n)) : go (n + 1) f

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f mapper = curry (zipWithOf func f (mapper . fst) (mapper . snd))

-- AdapterLike ((g a -> f b) -> (g s -> f t))
adaptT :: (Applicative g, Comonad g) => ((a -> f b) -> (s -> f t)) -> AdapterLike f g s t a b
adaptT l f s = (l (f . pure) . extract) s

adaptG :: (Applicative f, Comonad f) => ((g a -> b) -> (g s -> t)) -> AdapterLike f g s t a b
adaptG l f s = pure $ l (extract . f) s

_1G :: forall a b m. Monoid m => Grate (a, m) (b, m) a b
_1G = grate go
  where
    go :: (((a, m) -> a) -> b) -> (b, m)
    go indexer = ((indexer fst), mempty)

-- holyGrail :: [(Int, ())] -> [(Int, ())] -> [(Int, ())]
holyGrail :: Monoid m => Grate [(a, m)] [(b, m)] a b
holyGrail = cyclize . _1G

-- derivative :: Grate' (Double -> Double) Double
-- derivative = grate go
--   where
--     go :: (((Double -> Double) -> Double) -> Double) -> Double -> Double
--     go indexer n = indexer (nearby n)
--     nearby n f = f (n + 1) - f (n - 1)

-- (((Double -> Double) -> Double) -> (Double -> s) -> s
-- derivative :: GrateLike' ((->) Double) s Double -> (Double -> s) -> s
derivative :: Grate' s Double -> (Double -> s) -> (Double -> s)
derivative g gs d = g (nearby d) (gs)
  where
    nearby :: Double -> (Double -> Double) -> Double
    nearby n f = (f (n + 1) - f (n - 1)) / 2

-- >>> let dx = derivative represented (\x -> Pair (x**2) ((-x) **3))
-- >>> dx 3.25
-- Pair 6.5 (-32.6875)

data Pair a = Pair a a
    deriving (Functor, Show)

instance Distributive Pair where
    distribute = distributeRep

instance Representable Pair where
  type Rep Pair = Bool
  index (Pair a _) False = a
  index (Pair _ b) True = b
  tabulate f = Pair (f False) (f True)

represented :: forall f a b. Representable f => Grate (f a) (f b) a b
represented = grate go
  where
    go :: ((f a -> a) -> b) -> f b
    go indexer = tabulate (indexer . flip Rep.index)

-- fork :: forall s t a b a' b'. Grate s t a b -> Grate s t a' b' -> Grate s t (a, a') (b, b')
-- fork g1 g2 f gs =
--     where
--       degrated1 :: ((s -> a) -> b) -> t
--       degrated1 = degrating g1
--       degrated2 :: ((s -> a') -> b') -> t
--       degrated2 = degrating g2

x, y :: [Int]
x = [1..10]
y = [20..25]
