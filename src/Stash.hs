{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module Stash where

import Control.Lens
import Data.Data
import Control.Monad.State
import Data.List as L
import Data.Map as M
import Data.Maybe

data T a = T (T a) a (T a) | Leaf
    deriving (Data, Show)

-- custom :: (a -> Bool) -> (T a -> Bool) -> LensLike' (State Bool) (T a) a
-- custom p tp f Leaf = pure Leaf
-- custom p tp f (T l c r)
--   | p c = (T <$> custom p f l <*> pure c <*> custom p f r) <* put True
--   | otherwise = T <$> custom p f l <*> pure c <*> custom p f r



xs :: [(String, (String, (String, Int)))]
xs = [("nested", ("enum", ("here:", 2))), ("nested", ("enum", ("here:", 3))), ("nested", ("enum", ("here:", 3)))]

counter :: Map Int Int
counter = L.foldl' addEnumCount M.empty xs
  where
    addEnumCount :: Map Int Int -> (a, (a1, (a2, Int))) -> Map Int Int
    addEnumCount counts structure = M.alter (Just . (+1) . fromMaybe 0) (getEnum structure) counts

getEnum :: (a, (a1, (a2, b))) -> b
getEnum (_, (_, (_, enum))) = enum

counter' :: [(String, (String, (String, Int)))] -> Map Int Int
counter' = M.unionsWith (+) . fmap (\structure -> M.singleton (getEnum structure) 1)

data Triple a = Triple a a a
    deriving (Functor, Foldable, Traversable, Show)

instance Applicative Triple where
  pure a = Triple a a a
  Triple f g h <*> Triple a b c  = Triple (f a) (g b) (h c)

ys :: Triple [Int]
ys = pure [1, 2, 3]
