module Alternative where

import Control.Lens
import Control.Monad
import Data.Char
import Control.Applicative
import Data.Foldable
import Debug.Trace
import Data.Either.Validation
import Data.Semigroup
import Data.Functor
import Data.Functor.Compose
import Data.Monoid (Alt(..))

newtype AltConstF f a b = AltConstF {getAltConst :: (f a)}
    deriving stock Functor

instance (Alternative f) => Applicative (AltConstF f a) where
  pure a = AltConstF empty
  AltConstF fa <*> AltConstF fa' = AltConstF (fa *> fa')

instance (Alternative f) => Alternative (AltConstF f a) where
  empty = AltConstF empty
  AltConstF fa <|> AltConstF fb = AltConstF (fa <|> fb)

instance (Semigroup a, Applicative f) => Semigroup (AltConstF f a b) where
  AltConstF fa <> AltConstF fb = AltConstF $ liftA2 (<>) fa fb

instance (Monoid a, Applicative f) => Monoid (AltConstF f a b) where
  mempty = AltConstF (pure mempty)

instance Contravariant (AltConstF f a) where
  contramap _ (AltConstF fa) = AltConstF fa

forked :: (Alternative f, Traversable t, Applicative t) => LensLike f (t a) (t b) a b
forked f ta = pure <$> asum (fmap f ta)

filterFork :: forall f s t a b. (Foldable f, Applicative f, Traversable t, Alternative t) => LensLike f (t a) (t b) a b
filterFork f s = pure . getAlt . foldMap (Alt . pure @t) . foldMap toList $ (f <$> s)

monoFilterFork :: forall f s t a b. (Foldable f, Alternative f) => LensLike f (f a) (f b) a b
monoFilterFork f s = pure . asum $ fmap f s


branching :: (Alternative f) => (s -> [s]) -> LensLike f s t s t
branching branch f s = asum . fmap f . branch $ s

alternated :: (Foldable f, Alternative f) => LensLike f (f a) (f b) a b
alternated f s = pure . asum $ (f <$> toList s)

altOver :: Alternative f => (a -> b) -> LensLike f s t a b -> s -> f t
altOver f l = l %%~ (pure . f)

runAlt :: (Alternative f) => LensLike' (AltConstF f a) s a -> s -> f a
runAlt lns s = getAltConst $ lns (AltConstF . pure) s

alternatives :: LensLike' (AltConstF [] a) s a -> s -> [a]
alternatives lns s = getAltConst $ lns (AltConstF . pure) s

alternately :: (Alternative f) => LensLike f s t a b -> LensLike f s t a b -> LensLike f s t a b
alternately fld1 fld2 f s = fld1 f s <|> fld2 f s

testAlternately :: Alternative f => LensLike' f (a, a) a
testAlternately = _1 `alternately` _2

infixr 4 %|~
(%|~) :: Alternative f => LensLike f s t a b -> (a -> b) -> s -> f t
(%|~) = flip altOver

infixr 4 .|~
(.|~) :: Alternative f => LensLike f s t a b -> b -> s -> f t
(.|~) l b = altOver (const b) l

infixr 4 ^|?
(^|?) :: s -> LensLike' (Validation (Maybe (First a))) s a -> Maybe a
(^|?) s l = case l (Failure . Just . First) s of
    Success _ -> Nothing
    Failure a -> getFirst <$> a

-- >>> (5 :: Int) ^|? alternately (\n -> [n, n+1, n+2]) . guarding even
-- Just 6

infixr 4 ^|..
(^|..) :: s -> LensLike' (Validation [a]) s a -> [a]
(^|..) s l = case l (Failure . pure) s of
    Success _ -> []
    Failure a -> a

infixr 4 .|!
(.|!) :: Alternative f => LensLike f s t a a -> (a -> Bool) -> s -> f t
(.|!) l p = l (\a -> if p a then pure a else empty)

infixr 4 %%|
(%%|) :: Alternative f => LensLike f s t a b -> (a -> f b) -> s -> f t
(%%|) = (%%~)

-- >>> (5 :: Int) ^|.. alternately (\n -> [n, n+1, n+2, n+3]) . guarding even
-- [6,8]

-- | Unlike 'filtered', 'guarding' can fail an ENTIRE traversal fork, not just a single slice of it; e.g. it bubbles up to the last 'fork'.
guarding :: Alternative f => (a -> Bool) -> LensLike' f a a
guarding p f s | p s = f s
               | otherwise = empty

words' :: (Char, [String])
words' = ('a', ["hylo", "ana", "cata", "zygyz"])

testedFilter :: [] (Char, [String])
testedFilter = words' & _2 . forked .|! (\x -> x == reverse x)

testedFilterFork :: Maybe (Char, [String])
testedFilterFork = words' & _2 . filterFork .|! (\x -> x == reverse x)

testedMonoFilterFork' :: [] (Char, [String])
testedMonoFilterFork' = words' & _2 . monoFilterFork .|! (\x -> x == reverse x)

-- data T a = T a

-- >>> ('a', [Just "hi", Nothing]) & _2 . alternated .|! const False
-- [('a',[])]
-- >>> ('a', [Just "hi", Nothing]) & _2 . alternated .|! const True
-- [('a',[Just "hi",Nothing])]
-- >>> ('a', [Just "hi", Nothing]) & _2 . traversed . alternated .|!  const False
-- Just ('a',[Nothing,Nothing])
-- >>> ('a', [Just "hi", Nothing]) & _2 .  alternately' id .|!  const False
-- Nothing
-- >>> ('a', [Just "hi", Nothing]) & _2 .  alternately' id .|!  const True
-- Just ('a',"hi")
