module Witherable where

import Data.Witherable
import Control.Lens


filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p x | p x = Just x
filterMaybe _ _ = Nothing
