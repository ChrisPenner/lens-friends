{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
module Scrap where

import GHC.TypeLits
import Data.Kind

data SymbolOrType where
  S :: Symbol -> SymbolOrType
  T :: Type -> SymbolOrType

type family MyFamily (x :: SymbolOrType) where
  MyFamily (S s) = "got symbol"
  MyFamily (T t) = "got type"
