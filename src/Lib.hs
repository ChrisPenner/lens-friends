{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Text hiding (index, length)
import Data.String
import Text.RawString.QQ
import Data.Map as M
import Control.Monad.State
import Data.Monoid
import Data.Ord

infixl 8 ^!.
(^!.) :: (Applicative f, Monoid a) => s -> Fold s (f a) -> f a
s ^!. fld = getAp (s ^. fld . to Ap)

infixl 8 ^!..
(^!..) :: (Applicative f, Monoid a) => s -> Fold s (f a) -> f [a]
s ^!.. fld = sequenceA (s ^.. fld)
