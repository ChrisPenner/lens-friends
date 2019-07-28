{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Asym where

import Control.Lens
import Control.Applicative
import Data.Monoid

data User = User {_name :: String, _address :: String}
    deriving Show
newtype Form = Form [String]
    deriving (Show)
    deriving newtype (Semigroup, Monoid)
makeLenses ''User

injForm :: String -> Form
injForm = Form . pure

oneWay :: (a -> b) -> Lens a r b r
oneWay g f a = f (g a)

restrict :: Getter s a -> Lens s r a r
restrict l = oneWay (view l)

userName :: Lens User r String r
userName = oneWay _name

userAddress :: Lens User r String r
userAddress = oneWay _address

cap :: Traversal s t a b -> (a -> b) -> Traversal s t x y
cap l g _f s = l (pure . g) s

(<->) :: (Semigroup t, Applicative f) => LensLike f s t a b -> LensLike f s t a b -> LensLike f s t a b
(<->) alens blens f s = liftA2 (<>) (alens f s) (blens f s)

form :: Traversal User Form Form Form
form = cap userName injForm <-> cap userAddress injForm

joe :: User
joe = User "Joe" "Wallaby Way"

run :: LensLike Identity s a a a -> s -> a
run l = runIdentity . l pure
