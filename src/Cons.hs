{-# LANGUAGE RankNTypes #-}
module Cons where

import Data.Bifunctor
import Control.Applicative
import Control.Lens

traverseCons :: Cons s s a a => Traversal' s a
traverseCons f s = case uncons s of
    Just (a, rest) -> cons <$> f a <*> traverseCons f rest
    Nothing -> pure s

splitCons :: (AsEmpty s, Cons s s a a) => Int -> s -> (s, s)
splitCons 0 s = (Empty, s)
splitCons n s = case uncons s of
    Just (a, rest) -> first (cons a) $ splitCons (n - 1) rest
    Nothing -> (Empty, Empty)

-- _take ::
--   (Cons s s a a, AsEmpty s) =>
--   Int -> Traversal' s s
_take n f s = case splitCons n s of
    (prefix, suffix) -> liftA2 (<>) (f prefix) (pure suffix)

-- _drop ::
--   (Cons s s a a, AsEmpty s) =>
--   Int -> Traversal' s s
_drop n f s = case splitCons n s of
    (prefix, suffix) -> liftA2 (<>) (pure prefix) (f suffix)

-- appendCons :: Cons s s a a => s -> s -> s
-- appendCons = undefined

-- ByteString ByteString Word8 Word8Source#	 
-- Cons ByteString ByteString Word8 Word8Source#	 
-- Cons Text Text Char CharSource#	 
-- Cons Text Text Char CharSource#	 
-- Cons [a] [b] a bSource#	 
-- Cons (ZipList a) (ZipList b) a bSource#	 
-- Cons (Seq a) (Seq b) a bSource#	 
-- (Unbox a, Unbox b) => Cons (Vector a) (Vector b) a bSource#	 
-- (Storable a, Storable b) => Cons (Vector a) (Vector b) a bSource#	 
-- (Prim a, Prim b) => Cons (Vector a) (Vector b) a bSource#	 
-- Cons (Vector a) (Vector b) a bSource#	 
-- Cons (Deque a) 
