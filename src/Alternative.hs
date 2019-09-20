module NonDet where

import Control.Lens
import Control.Monad
import Data.Char
import Control.Applicative
import Data.Foldable
import Debug.Trace

forked :: (Foldable f, Alternative f) => (s -> [s]) -> LensLike' f s s
forked fork' f s = asum $ fmap f (fork' s)

-- forked :: Alternative f => (s -> f s) -> LensLike' f s s
-- forked fork' f s = f (fork' s)


nonDetOver :: Alternative f => (a -> a) -> LensLike' f s a -> s -> f s
nonDetOver f l = l (pure . f)

test :: [(String, Integer)]
test = nonDetOver (const 'x') (_1 . forked (\x -> [x, map toUpper x]) . taking 3 traverse) $ ("testing", 3)

infixr 4 %|~
(%|~) :: Alternative f => LensLike' f s a -> (a -> a) -> s -> f s
(%|~) = flip nonDetOver

infixr 4 .|~
(.|~) :: Alternative f => LensLike' f s a -> a -> s -> f s
(.|~) l b = nonDetOver (const b) l

guarding :: Alternative f => (a -> Bool) -> LensLike' f a a
guarding p f s | p s = f s
               | otherwise = empty

otherTest :: Maybe (String, Int)
otherTest = ("testing", 3) & (_1 . forked (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isUpper) .|~ 'c'

guardTest :: [(String, Int)]
guardTest = ("testing", 3) & (_1 . forked (\x -> [x, map toUpper x]) . taking 3 traverse) . guarding (isLower) %|~ const 'c'
