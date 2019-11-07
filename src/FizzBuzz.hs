{-# LANGUAGE DeriveDataTypeable #-}
module FizzBuzz where

import Control.Lens
import Unsafe.Coerce
import Data.Foldable
import Text.RawString.QQ (r)
import qualified Data.Text as T
import Data.Aeson.Lens
import Data.Aeson
import Data.Data

sample :: (MyType, [(String, Int)])
sample = (MyType 1 "something", [("One", 1), ("Two", 2), ("Three", 3)])

data MyType = MyType Int String
  deriving (Data, Show)

-- j :: String
-- j = [r|
-- {
--    "id":"nginx-mysql",
--    "kind": "Pod",
--    "apiVersion": "v1",
--    "spec": {
--       "containers": [
--          {
--             "name": "load balancer 1",
--             "image": "nginx",
--             "port": 80
--          },
--          {
--             "name": "load balancer 2",
--             "image": "nginx",
--             "port": 442
--          },
--          {
--             "name": "orders db",
--             "image": "mysql",
--             "port": 80
--          },
--          {
--             "name": "app",
--             "image": "my-app",
--             "port": 80
--          },
--          {
--             "name": "logging",
--             "image": "kibana",
--             "port": 442
--          },
--          {
--             "name": "users db",
--             "image": "mysql",
--             "port": 442
--          }
--       ]
--    }
-- }
-- |]


case_ :: Prism s t a b -> (a -> c) -> (t -> c) -> s -> c
case_ p f g = either g f . matching p

_DividedBy :: Int -> Prism' Int Int
_DividedBy n = prism' embed match
  where
    embed x = x * n
    match x
      | x `mod` n == 0 = Just (x `div` n)
      | otherwise = Nothing

otherwise_ :: a -> a
otherwise_ = id

fizzbuzz' :: Int -> String
fizzbuzz' =
    case_ (_DividedBy 3 . _DividedBy 5) (const " FizzBuzz")
  $ case_ (_DividedBy 3) (const "Fizz")
  $ case_ (_DividedBy 5) (const "Buzz")
  $ otherwise_ show

fizzbuzz :: Int -> String
fizzbuzz = show & outside (_DividedBy 3) . mapped .~ "Fizz"
                & outside (_DividedBy 5) . mapped .~ "Buzz"
                & outside (_DividedBy 15) . mapped .~ "FizzBuzz"

-- >>> traverse_ (putStrLn . fizzbuzz) [1..20]
-- 1
-- 2
-- Fizz
-- 4
-- Buzz
-- Fizz
-- 7
-- 8
-- Fizz
-- Buzz
-- 11
-- Fizz
-- 13
-- 14
-- FizzBuzz
-- 16
-- 17
-- Fizz
-- 19
-- Buzz
