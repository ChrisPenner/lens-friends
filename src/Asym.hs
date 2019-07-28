{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Asym where

import Control.Lens
import Control.Applicative
import Data.Monoid
import Data.Void

data User = User {_name :: String, _address :: String}
    deriving Show
newtype Form = Form [String]
    deriving (Show)
    deriving newtype (Semigroup, Monoid)
makeLenses ''User

type Refraction f r s a = LensLike f s r a r
type Refraction' f r s = Refraction f r s Void

injForm :: String -> Form
injForm = Form . pure

oneWay :: (a -> b) -> Lens a r b r
oneWay g f a = f (g a)

restrict :: Functor f => Getter s a -> Refraction f r s a
restrict l = oneWay (view l)

userName :: Functor f => Refraction f r User String
userName = restrict name

userAddress :: Functor f => Refraction f r User String
userAddress = restrict address

inject :: Functor f => (s -> r -> r) -> Refraction f r s s
inject g f s = g s <$> f s

cap :: Traversal s t a b -> (a -> b) -> Traversal s t x y
cap l g _f s = l (pure . g) s

capped :: Applicative f => (s -> r) -> Refraction f r s x
capped g f s = pure $ g s

capped' :: (Monoid r, Applicative f) => Refraction f r s Void
capped' f s = pure mempty

capped'' :: Applicative f => Refraction f r r ()
capped'' = capped id

infixr 8 <->
(<->) :: (Semigroup t, Applicative f) => LensLike f s t a b -> LensLike f s t a b -> LensLike f s t a b
(<->) alens blens f s = liftA2 (<>) (alens f s) (blens f s)

form :: Applicative f => Refraction f Form User Void
form = userName . capped injForm <-> userAddress . inject appendForm . capped'

appendForm :: String -> Form -> Form
appendForm s f = Form [s] <> f

joe :: User
joe = User "Joe" "Wallaby Way"

run :: Refraction Identity a s a -> s -> a
run l = runIdentity . l pure

run' :: Refraction Identity a s Void -> s -> a
run' l = runIdentity . l absurd

