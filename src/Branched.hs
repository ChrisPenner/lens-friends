{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Branched where

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

-- > example
-- [ Left "1", Left "olleh", Right 100, Right 5
-- , Left "2", Left "dlrow", Right 200, Right 5 ]

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
