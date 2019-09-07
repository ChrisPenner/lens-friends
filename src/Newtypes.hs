{-# LANGUAGE TypeFamilies #-}
module Newtypes where

import Control.Lens
import GHC.Generics

newtype A = A [Int]
    deriving stock (Generic)
deriving instance Wrapped A

newtypeFolded :: (Wrapped s, Unwrapped s ~ f a, Foldable f)
              => Fold s a
newtypeFolded = _Wrapped' . folded

-- x = sumOf (_Wrapped' . folded) $ A [1, 2, 3]
