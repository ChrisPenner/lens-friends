{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Text hiding (index, length)
import Data.String
import Text.RawString.QQ
import Data.Map as M
import Data.Bifunctor.Join

-- | branched :: Functor f => (f (Getter s a)) -> Getter s (f a)
-- λ> [(1, "test"), (2, "three")] ^. branched [folded . _2, folded . _1 . re _Show, ix 1 . _2]
-- ["testthree","12","three"]
-- branched ::
--   (Profunctor p, Contravariant f1, Functor f2) =>
--   f2 (Getting b s b) -> Optic' p f1 s (f2 b)
-- branched :: Functor f => (f (Getter s a)) -> Getter s (f a)
branched getters = to go
  where
    go s = flip view s <$> getters

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

> example
[ Left "1", Left "olleh", Right 100, Right 5
, Left "2", Left "dlrow", Right 200, Right 5 ]

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
    & (reindexed (^?! key "image" . _String)
        (key "spec"
    . key "containers"
    . values
    . selfIndex)
    . index "mysql"
    -- . filteredHaving (key "port" . _Number . only 80)
    <. (key "name" . _String)
      )
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


data Pair a = Pair  a a
  deriving (Functor, Show)

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
