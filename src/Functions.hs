module Functions where

import Control.Lens
import Data.Maybe

validateAtZero :: (Int -> Int) -> Maybe (Int -> Int)
validateAtZero = ix 0 %%~ \result -> if result < 0 then Nothing else Just result

-- >>> isJust $ validateAtZero (+10)
-- True
-- >>> isJust $ validateAtZero (subtract 10)
-- False
