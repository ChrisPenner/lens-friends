{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Text as T hiding (index, length)
import Data.String
import Text.RawString.QQ
import Data.Map as M
import Data.Bifunctor.Join
import Control.Comonad
import Control.Monad.State
import Data.Monoid
import Data.Ord
import Branched hiding (users)
import Data.Function
import Data.Char as Char
import Data.List as L
import Control.Lens.Regex

infixl 8 ^!.
(^!.) :: (Applicative f, Monoid a) => s -> Fold s (f a) -> f a
s ^!. fld = getAp (s ^. fld . to Ap)

test1 = [(1 :: Int, "hi" :: String), (2, "there")]
      ^. branched go
  where
    go :: Int -> Getter [(Int, String)] String
    go n = singular (ix n . _2)

-- test2 :: [[String]]
example = [(1 :: Int, "hello" :: String), (2, "world")]
      ^. traversed
      . branched [ _1 . to show   . re _Left
                 , _2 . reversed  . re _Left
                 , _1 . to (*100) . re _Right
                 , _2 . to length . re _Right
                 ]

-- > example
-- [ Left "1", Left "olleh", Right 100, Right 5
-- , Left "2", Left "dlrow", Right 200, Right 5 ]

-- Unify the left side of a Bitraversable with the right, then traverse both
matchLeft t = beside t id
-- Unify the right side of a Bitraversable with the left, then traverse both
matchRight t = beside id t

-- Filter for elements which have at least one target for the given fold
-- Use with 'only' for equality checks
filteredHaving :: (Choice p, Applicative f) => Fold s a -> Optic' p f s s
filteredHaving f = filtered (has f)

-- | Use the result of a getter over the current focus as the index
settingIndexOf :: Getter s i -> IndexedLens' i s s
settingIndexOf getter p s = indexed p (s ^. getter) s

-- | Set the index to some transformation of the input
-- Doesn't alter the focus
settingIndex :: (s -> i) -> IndexedLens' i s s
settingIndex getter p s = indexed p (getter s) s

xs :: [(String, String)]
xs = [("spot", "dog"), ("dingo", "bird"), ("spot", "cat"), ("calypso", "goldfish")]


inp :: Value
inp = view (singular $ _JSON @String) [r|
{
   "id":"nginx-mysql",
   "kind": "Pod",
   "apiVersion": "v1",
   "spec": {
      "containers": [
         {
            "name": "load balancer 1",
            "image": "nginx",
            "port": 80
         },
         {
            "name": "load balancer 2",
            "image": "nginx",
            "port": 442
         },
         {
            "name": "orders db",
            "image": "mysql",
            "port": 80
         },
         {
            "name": "app",
            "image": "my-app",
            "port": 80
         },
         {
            "name": "logging",
            "image": "kibana",
            "port": 442
         },
         {
            "name": "users db",
            "image": "mysql",
            "port": 442
         }
      ]
   }
}
|]

query :: [Text]
query =
    inp
    ^.. key "spec"
    . key "containers"
    . values
    . settingIndexOf (singular $ key "image")
    . index "mysql"
    . filteredHaving (key "port" . _Number . only 80)
    . key "name"
    . _String
-- ["orders db"]
--

mutated =
    inp
    & key "spec"
    . key "containers"
    . values
    . reindexed (^?! key "image" . _String) selfIndex
    . index "mysql"
    -- . filteredHaving (key "port" . _Number . only 80)
    . cloneIndexPreservingTraversal (key "name" . _String)
    %@~ (<>)


-- fromString :: String -> Text -> Text -> Text

instance IsString (Text -> Text -> Text) where
    fromString a b c = b <> pack a <> c

withImageName :: Value
withImageName =
    inp
    & key "spec"
    . key "containers"
    . values
    . settingIndexOf (singular $ key "image" . _String)
    <. key "name"
    . _String
    %@~ "-"


test3 = inp
  ^.. key "spec"
   . key "containers"
   . values
   . branched (Pair (key "name" . _String) (key "image" . _String))


users :: Value
users = view (singular $ _JSON @String) [r|
{ "users" :
       [  { "name" : "John Wick"
          , "profile" : { "pet" : { "name" : "Tito" }}
       }, { "name" : "Jon Arbuckle"
          , "profile" : { "pet" : { "name" : "Garfield" }}
}]}
|]

collectUsers :: [Pair Text]
collectUsers =
  users
    ^.. key "users" . values
      . branched
          (Pair (key "name" . _String)
                (key "profile" . key "pet" . key "name" . _String))


extended :: (Comonad w, Traversable w) => Traversal (w a) (w b) (w a) b
extended f w = sequenceA $ extend f w

splitting :: forall a b. (a -> Bool) -> Traversal a b (Either a a) (Either b b)
splitting p f t = either id id <$> choose t
  where
    choose a | p a = f (Right a)
             | otherwise = f (Left a)

splitting' :: forall a b. (a -> Bool) -> Traversal a b (Either a a) b
splitting' p f t = choose t
  where
    choose a | p a = f (Right a)
             | otherwise = f (Left a)
infixl 8 ^!..
(^!..) :: (Applicative f, Monoid a) => s -> Fold s (f a) -> f [a]
s ^!.. fld = sequenceA (s ^.. fld)

newtype Err = Err String
  deriving (Semigroup, Monoid, Show) via String


args = ["1", "2", "3", "4"]

-- >>> args ^?! (_tail . _head . _Show `failing` like 0)
-- 2
-- >>> args ^?! (ix 99 . _Show `failing` like 0)
-- 0


snakeToTitle :: T.Text -> T.Text
snakeToTitle = regex [rx|_\w|] . match %~ T.tail . T.toUpper
-- snakeToTitle s = L.groupBy ((==) `on` (== '_')) s ^. traversed . filtered (/="_") . to (_head %~ Char.toUpper)
