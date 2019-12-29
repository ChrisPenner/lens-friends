{-# LANGUAGE LambdaCase #-}
module State where

import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe

myMap :: M.Map Char Int
myMap = M.fromList [('a', 10), ('b', 20)]

test :: State (M.Map Char Int) Int
test = do
    zoom (at 'a' . non 42) (id <+= 1)
    -- at 'c' %%= \case
    --   Just x -> (x + 1, Just (x + 1))
    --   Nothing -> (42 + 1, Nothing)


test' :: State (M.Map Char Int) ()
test' = do
    ix 'c' <~ use (at 'c' . non 42 . to (*10))

go' :: Int -> State String a -> State (M.Map Int String, String) a
go' i ms = do
    def <- use _2
    zoom (_1 . at i . non def) ms

-- >>> runState test' myMap
-- (43,fromList [('a',10),('b',20)])

-- >>> runState test myMap
-- (43,fromList [('a',10),('b',20)])

-- >>> runState test myMap
-- (43,fromList [('a',10),('b',20),('c',43)])

-- >>> runState test myMap
-- (11,fromList [('a',11),('b',20)])
