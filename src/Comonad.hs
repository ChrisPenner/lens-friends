{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Comonad where

import Control.Lens
import Control.Arrow
import Data.Monoid

import Control.Comonad
import Control.Comonad.Store
import Data.List.NonEmpty

zoomExtend :: ComonadApply w => Lens s t a b -> (w a -> b) -> w s -> w t
zoomExtend l f ws = liftW2 (set l) (extend f wa)  ws
  where
    wa = viewer l <$> ws
    viewer :: forall s t a b. Lens s t a b -> s -> a
    viewer l s = getConst $ l Const s


st :: Store (Sum Int) (Int, String)
st = store (getSum &&& show . getSum) 1

-- ex :: NonEmpty (Int, String)
-- ex = (id &&& show) <$> 0 :| [1..5]

ex' = zoomExtend _1 (peeks (+1)) st
