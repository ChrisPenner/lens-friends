{-# LANGUAGE TypeFamilies #-}
module Grate where

import Control.Lens as L
import Lens.Family2 hiding (zipWithOf)
import Lens.Family2.Unchecked
import Data.Function as F
import Data.Maybe
import Control.Comonad
import Control.Comonad.Traced
import Control.Comonad.Store
import Control.Comonad.Env
import Control.Monad
import Data.Distributive
import Data.Functor.Rep as Rep
import qualified Data.List.NonEmpty as NE
import Data.Foldable as F
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

-- extendy :: Comonad w => Lens' s a -> GrateLike' w s a
-- extendy l f gs =

-- extendThrough :: ComonadApply w => L.Lens' s a -> (w a -> a) -> w s -> w s
-- extendThrough l f w = liftW2 (L.set l) (extend f (L.view l <$> w)) w

extendOf :: ComonadApply w => L.Lens s t a b -> (w a -> b) -> w s -> w t
extendOf l f w = liftW2 (L.set l) (extend f (viewer l <$> w)) w
  where
    viewer :: L.Lens s t a b -> s -> a
    viewer l = getConst . l Const

-- bindOn :: Monad m => L.Lens s t a b -> (a -> m b) -> m s -> m t
-- bindOn l f m = m >>= \s -> L.set l <$> (f $ viewer l s) <*> pure s
--   where
--     viewer :: L.Lens s t a b -> s -> a
--     viewer l = getConst . l Const

bindOf :: Monad m => L.Traversal s t a b -> (a -> m b) -> m s -> m t
bindOf l f m = m >>= l %%~ f

extendThrough :: forall s t a b w. Comonad w => Grate s t a b -> (w a -> b) -> w s -> w t
extendThrough g f = extend (degrated . helper)
  where
    helper :: w s -> (s -> a) -> b
    helper w' sToA = f (sToA <$> w')
    degrated :: ((s -> a) -> b) -> t
    degrated = degrating g

(-<) :: Comonad w => Grate s t a b -> (w a -> b) -> w s -> w t
(-<) = extendThrough

-- withContext :: Representable f => Iso' s (a, s') -> GrateLike' f s a -> GrateLike' f s a
withContext :: Representable f => (s -> s -> s) -> GrateLike' f s a -> GrateLike' f s a
withContext i gr f gs = undefined (gr f gs)
-- fmap (L.view (from i))


-- fixed :: forall f s t a b. Representable f => Grate' s t a b -> (Rep f -> a -> (Rep f -> b) -> b) -> f s -> f t
-- fixed gr f fs = degrated (helper undefined)
--   where
--     helper :: (s -> a) -> b
--     helper sToA = undefined
--     degrated :: ((s -> a) -> b) -> t
--     degrated = degrating g

-- fixed :: forall f s t a b. Representable f => (Rep f -> a -> (Rep f -> b) -> b) -> Grate (f a) (f b) a b
-- fixed f fs gs = degrated (helper undefined)
--   where
--     helper :: (f a -> a) -> b
--     helper faToA = faToA (tabulate . flip Rep.index)
--     degrated :: ((f a -> a) -> b) -> f b
--     degrated = degrating g

-- factorial :: Int -> Int
-- factorial

factorialFix :: ((Int -> Int) -> Int -> Int)
factorialFix f 0 = 1
factorialFix f n = n * f (n - 1)

testFixed :: (a -> a -> Int) -> (Int -> a) -> (Int -> a) -> (Int -> Int)
testFixed = zipWithOf (fixed factorialFix)

-- testFixedAgain :: (a -> a -> Int) -> (Int -> a) -> (Int -> a) -> (Int -> Int)
-- testFixedAgain = zipWithOf (fixed (represented . factorialFix))


testFixed' :: (Int -> [a]) -> (Int -> [a]) -> (Int -> Int)
testFixed' = zipWithOf (fixed factorialFix) (\x y -> length x)

fixed :: forall f a b. Representable f => ((Rep f -> b) -> Rep f -> b) -> Grate (f a) (f b) a b
fixed fixer = grate go
  where
    repper :: f b -> Rep f -> b
    repper fb r = fixer (Rep.index fb) r
    go :: ((f a -> a) -> b) -> f b
    go indexer = fix $ \fb -> tabulate (repper fb)

exampleExtendThrough :: NE.NonEmpty (Pair String)
exampleExtendThrough =
    neList
    L.& represented -< fold
-- Pair "abc" "ABC" :| [Pair "bc" "BC",Pair "c" "C"]

tracesing :: ComonadTraced m w => (s -> m) -> GrateLike w s s s s
tracesing f = extending (traces f)

tracing :: ComonadTraced m w => m -> GrateLike w s s s s
tracing m = extending (trace m)

extendingThrough :: (Comonad w) => (w a -> b) -> Grate s s a b -> GrateLike' w s s
extendingThrough f gr g wa = g $ extendThrough gr f wa

extending :: (Comonad w) => (w a -> b) -> GrateLike w a t b t
extending f g wa = g $ extend f wa

experimenting :: (Functor f, ComonadStore i w) => (i -> f i) -> GrateLike w s x (f s) x
experimenting f = extending (experiment f)

testExperimenting :: Pair (Pair Int)
testExperimenting = (represented . experimenting (\x -> Pair (x -1) (x+1)) . represented) extract pairStore

pairStore :: Store Int (Pair Int)
pairStore = store (\x -> Pair x (-x)) 10

neList :: NE.NonEmpty (Pair String)
neList = Pair "a" "A" NE.:| [Pair "b" "B",  Pair "c" "C"]

neTuples :: NE.NonEmpty (String, String)
neTuples = ("a", "A") NE.:| [("b", "B"),  ("c", "C")]


-- regrate :: Grate' s a -> s -> Grate' s s
-- regrate gr f s = undefined

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
