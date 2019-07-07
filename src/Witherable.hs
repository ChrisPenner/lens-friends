module Witherable where

import Data.Witherable as W
import Control.Lens
import Data.Map as M


filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p x | p x = Just x
filterMaybe _ _ = Nothing

sampleMap :: Map [Char] (String, Int)
sampleMap = M.fromList [("george", ("abc", 1)), ("sandy", ("abc", 2)), ("metz", ("abd", 3))]


witherPred :: (Applicative f) => (a -> Bool) -> (a -> f (Maybe a))
witherPred p = pure . go
  where
    go x
        | p x = Just x
        | otherwise = Nothing
