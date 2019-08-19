{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Junk where

import Control.Lens as L
import Data.Void
import Data.Functor.Contravariant
import Control.Monad.Writer


data User = User { name :: String, age :: Int, country :: String, pets :: [String]}
    deriving Show

-- Traversal s t a b= (a -> f b) -> s -> f t
-- Traversal User User String String = (a -> f b) -> s -> f t
-- Traversal' User String = (String -> f String) -> User -> f User

-- userStrings :: Applicative f => (String -> f String) -> User -> f User
-- userStrings f u = User <$> f (name u) <*> pure (age u) <*> f (country u)


exUser :: User
exUser = User "joe" 34 "Samoan" ["Garfield"]

-- userStrings :: Applicative f => Bool -> (String -> f String) -> User -> f User
-- userStrings True f u = User <$> f (name u) <*> pure (age u) <*> pure (country u)
-- userStrings False f u = User <$> pure (name u) <*> pure (age u) <*> f (country u)


userStrings :: Functor f => (String -> f String) -> User -> f User
userStrings f u = f (name u) <&> \newName -> u{name=newName}

set :: (forall f. Functor f => (String -> f String) -> User -> f User) -> String -> User -> User
set l newString u = runIdentity $ l (const (Identity newString)) u

view :: (forall f. Functor f => (String -> f String) -> User -> f User) -> User -> String
view l u = getConst $ l Const u

toListOf' :: (forall f. Functor f => (a -> f b) -> s -> f t) -> s -> [a]
toListOf' l u = getConst $ l (Const . pure) u

something :: Functor g => (forall f. Functor f => (String -> f String) -> User -> f User) -> (String -> g String) -> User -> g User
something = id

set' :: (forall f. Functor f => (String -> f String) -> User -> f User) -> String -> User -> User
set' l newString u = runIdentity $ l (const (Identity newString)) u

data Complex = Complex Int Bool String
    deriving Show
data Simp = Simp Int
    deriving Show

-- transformer :: Lens' Complex Simple
transformer :: Functor f => (Simp -> f Simp) -> Complex -> f Complex
transformer f (Complex i b s) = f (Simp i) <&> \(Simp si) -> Complex si b s

-- stringFold :: Fold User String
stringFold :: (Applicative f, Contravariant f) => (String -> f Void) -> User -> f Void
stringFold f (User name' age' country' pets') = phantom (f name') <*> phantom (f country') <*> phantom (traverse f pets')

-- Complex = Complex (Complex) Int String
-- Simple = Simple Int String


data BT a = BT (BT a) a (BT a) | Leaf
    deriving Show

-- nodes :: Traversal' (BT a) (BT a)
nodes :: Monad f => (BT a -> f (BT a)) -> BT a -> f (BT a)
nodes f Leaf = f Leaf
nodes f (BT l c r) = do
    lefts <- nodes f l
    rights <- nodes f r
    f (BT lefts c rights)

exBin :: BT Int
exBin = BT (BT Leaf 1 Leaf) 2 (BT Leaf 3 Leaf)

-- betterListOf :: LensLike' (Writer [a]) s a -> s -> [a]
betterListOf :: forall s a. ((a -> Writer [a] a) -> (s -> Writer [a] s)) -> s -> [a]
betterListOf l s = execWriter $ l go s
  where
    go :: a -> Writer [a] a
    go a = tell [a] >> return a


data Thermostat = Thermostat Double
    deriving Show

startThermo :: Thermostat
startThermo = Thermostat (20)

kelvin :: Lens' Thermostat Double
kelvin = lens getter setter
  where
    getter (Thermostat n) = max 0 n
    setter (Thermostat _) n' = Thermostat (max 0 n')


celsius :: Lens' Thermostat Double
celsius = kelvin . iso (* 3074) (/ 3074)




-- (p Simp (f Simp)) -> (p Complex (f Complex))

-- AReview = (Tagged a (f b)) -> (Tagged s (f t))

-- Fold s a = (Functor f, Contravariant f) => (a -> (f a)) -> s -> f s

-- review

-- foldOf' :: Fold s a -> s ->

-- data SimpleThing a = SimpleThing a

-- Lens Int Integer String ByteString
-- Lens s t a b

-- (a -> f b) -> s -> f t

-- (String -> f ByteString) -> Int -> f Integer





-- userStrings :: Applicative f => (String -> f String) -> User -> f User
-- userStrings f u =
--     User <$> f (name u)
--          <*> pure (age u)
--          <*> f (country u)
--          <*> (traversed . indices even) f (pets u)



