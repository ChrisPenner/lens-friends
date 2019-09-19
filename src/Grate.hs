module Grate where

import Control.Lens as L
import Lens.Family2 hiding (zipWithOf)
import Lens.Family2.Unchecked
import Data.Function as F
import Data.Maybe
import Control.Comonad
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

cyclize :: Int -> Grate [a] [b] a b
cyclize n = grate (go 0)
  where
    go :: Int -> (([a] -> a) -> b) -> [b]
    go n' f | n == n' =  f ((^?! ix n') . cycle) : go (n' + 1) f
    go n' f =  f ((^?! ix n') . cycle) : go (n' + 1) f

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

x, y :: [Int]
x = [1..10]
y = [20..25]
