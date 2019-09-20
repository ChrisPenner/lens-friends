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

alternately :: (Alternative f, Foldable f) => (s -> f a) -> LensLike f s t a t
alternately fork' f s = asum $ fmap f (fork' s)

-- No good?
alternately' :: (Foldable g, Alternative f, Monad f) => (s -> g (f a)) -> LensLike f s t a t
alternately' fork' f s = asum (fork' s) >>= f

alternated :: (Foldable f, Alternative f) => LensLike f (f a) (f b) a b
alternated f s = pure . asum $ (f <$> toList s)

nonDetOver :: Alternative f => (a -> b) -> LensLike f s t a b -> s -> f t
nonDetOver f l = l %%~ (pure . f)

test :: [(String, Integer)]
test = nonDetOver (const 'x') (_1 . alternately (\x -> [x, map toUpper x]) . taking 3 traverse) $ ("testing", 3)

infixr 4 %|~
(%|~) :: Alternative f => LensLike f s t a b -> (a -> b) -> s -> f t
(%|~) = flip nonDetOver

infixr 4 .|~
(.|~) :: Alternative f => LensLike f s t a b -> b -> s -> f t
(.|~) l b = nonDetOver (const b) l

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

otherTest :: [(String, Int)]
otherTest = ("testing", 3) & (_1 . alternately (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isUpper) .|~ 'c'

guardTest :: [(String, Int)]
guardTest = ("testing", 3) & (_1 . alternately (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isLower) %|~ const 'c'


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
