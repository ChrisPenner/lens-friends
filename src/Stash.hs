{-# LANGUAGE DeriveDataTypeable #-}
module Stash where

import Control.Lens
import Data.Data
import Control.Monad.State

data T a = T (T a) a (T a) | Leaf
    deriving (Data, Show)

-- custom :: (a -> Bool) -> (T a -> Bool) -> LensLike' (State Bool) (T a) a
-- custom p tp f Leaf = pure Leaf
-- custom p tp f (T l c r)
--   | p c = (T <$> custom p f l <*> pure c <*> custom p f r) <* put True
--   | otherwise = T <$> custom p f l <*> pure c <*> custom p f r

