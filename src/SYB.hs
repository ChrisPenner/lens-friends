{-# LANGUAGE DerivingVia #-}
module SYB where

import Control.Lens
import Control.Lens.Plated
import Data.Data.Lens
import Data.Tree
import Data.Tree.Lens
import Data.Data
import Data.Functor.Contravariant
import Control.Applicative
import Data.Foldable

tree :: Tree Int
tree = Node 1 [Node 2 [Node 4 [Node 5 []]], Node 3 [Node 40 [], Node 50 []]]
-- tree = Node 1 [Node 2 [Node 3 []], Node 5 []]

-- Good use for an Alternative style optic?
-- pathTo :: Fold s s -> Fold s a -> Fold s s
-- pathTo children focus f s | has focus s = phantom $ pure ()
-- pathTo children focus f s = f s *> (children . pathTo children focus) f s

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

-- pathTo :: Fold s s -> Fold s a -> LensLike' (AltConstF [] s) s s
-- pathTo children focus f s | has focus s = phantom $ pure ()
-- pathTo children focus f s | hasn't children s = empty
-- pathTo children focus f s = (AltConstF [s]) *> (alternating (children . pathTo children focus) f s

alternately :: (Alternative f) => LensLike f s t a b -> LensLike f s t a b -> LensLike f s t a b
alternately fld1 fld2 f s = fld1 f s <|> fld2 f s

testAlternately :: Alternative f => LensLike' f (a, a) a
testAlternately = _1 `alternately` _2

allternatives :: Alternative f => LensLike f s t a b -> (a -> b) -> s -> f t
allternatives lns f s = lns (pure . f) s

guarded :: (Alternative f) => (s -> Bool) -> LensLike' f s s
guarded p f s | p s = f s
              | otherwise = empty

runAlt :: (Alternative f) => LensLike' (AltConstF f a) s a -> s -> f a
runAlt lns s = getAltConst $ lns (AltConstF . pure) s

alternatives :: LensLike' (AltConstF [] a) s a -> s -> [a]
alternatives lns s = getAltConst $ lns (AltConstF . pure) s


-- pathTo children focus s = asumOf (children .

-- xs :: [[[Int]]]
-- xs = [[[1, 2, 3], [4, 5, 6]]]



-- How can we keep track of the index of this whole thing regardless of the filter
-- biplate . deepOf uniplate (taking 1 $ filtered p)

cata :: (Data a, Monad m) => LensLike' m a a
cata = transformMOf uniplate
