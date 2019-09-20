module Alternative where

import Control.Lens
import Control.Monad
import Data.Char
import Control.Applicative
import Data.Foldable
import Debug.Trace
import Data.Either.Validation
import Data.Semigroup

alternately :: (Alternative f) => (s -> [a]) -> LensLike f s t a t
alternately fork' f s = asum $ fmap f (fork' s)

alternately' :: (Alternative f, Monad f) => (s -> [f a]) -> LensLike f s t a t
alternately' fork' f s = asum (fork' s) >>= f

nonDetOver :: Alternative f => (a -> b) -> LensLike f s t a b -> s -> f t
nonDetOver f l = l (pure . f)

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

-- >>> (5 :: Int) ^|.. alternately (\n -> [n, n+1, n+2, n+3]) . guarding even
-- [6,8]

-- | Unlike 'filtered', 'guarding' can fail an ENTIRE traversal fork, not just a single slice of it; e.g. it bubbles up to the last 'fork'.
guarding :: Alternative f => (a -> Bool) -> LensLike' f a a
guarding p f s | p s = f s
               | otherwise = empty

otherTest :: Maybe (String, Int)
otherTest = ("testing", 3) & (_1 . alternately (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isUpper) .|~ 'c'

guardTest :: [(String, Int)]
guardTest = ("testing", 3) & (_1 . alternately (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isLower) %|~ const 'c'
