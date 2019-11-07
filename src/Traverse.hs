module Traverse where

import Control.Lens


--You can use *most* optics as though they're a custom `traverse`

-- >>> (each . _Right) print (Left 1, Right 2, Left 3)
--2
---- We can ignore the new structure it returns
--(Left 1,Right (),Left 3)

--It works on other types of effects too!
--Here we use lists as a non-determinism effect to get all possible combos!

-- >>> each (\x -> [x, x+5]) (0, 10)
--[(0,10),(0,15),(5,10),(5,15)]

---- Helper
--safeHead :: [a] -> Maybe a
--safeHead (x:_) = Just x
--safeHead _ = Nothing

--If using them directly feels weird, we can use traverseOf to make it explicit:

-- >>> traverseOf (traversed . _2) safeHead [(1, "one"), (2, "two")]
--Just [(1,'o'),(2,'t')]
----
-- >>> traverseOf (traversed . _2) safeHead [(1, ""), (2, "two")]
--Nothing

--`%%~` is the infix version of `traverseOf`:

-- >>> [(1, "one"), (2, "two")] & traversed . _2 %%~ safeHead
--Just [(1,'o'),(2,'t')]

-- >>> [(1, ""), (2, "two")] & traversed . _2 %%~ safeHead
--Nothing
