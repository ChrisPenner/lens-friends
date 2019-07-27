{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Scrap where

import GHC.TypeLits
import Data.Kind
import Control.Lens
import Data.Word
import Data.Bits.Lens

data SymbolOrType where
  S :: Symbol -> SymbolOrType
  T :: Type -> SymbolOrType

type family MyFamily (x :: SymbolOrType) where
  MyFamily (S s) = "got symbol"
  MyFamily (T t) = "got type"

data Blah = Blah Int Int

makeLenses ''Blah

structure :: (forall a. g a -> f (g a)) -> g x -> f (g x)
structure = id

test :: String -> Word8 -> Bool
test s n = map (=='1') s == n ^.. bits

