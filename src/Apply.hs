{-# LANGUAGE RankNTypes #-}
module Apply where

import Control.Lens
import Data.Functor.Contravariant
import Data.Aeson.Lens
import Control.Applicative

(<^$>) :: (a -> b) -> Fold s a -> Fold s b
(<^$>) f lns = lns . to f

(<^*>) :: Getting (Maybe (a -> b)) s (a -> b) -> Getting (Maybe a) s a -> Fold s b
(<^*>) = liftT2 ($)

liftT2 :: (a -> b -> c) -> Getting (Maybe a) s a -> Getting (Maybe b) s b -> Fold s c
liftT2 f t1 t2 fc s =
    case (s & t1 %%~ Const . Just, s & t2 %%~ Const . Just) of
        (Const (Just a), Const (Just b)) -> phantom $ fc (f a b)
        _ -> pure s

liftT3 :: (a -> b -> c -> d) -> Getting (Maybe a) s a -> Getting (Maybe b) s b -> Getting (Maybe c) s c -> Fold s d
liftT3 f t1 t2 t3 fd s =
    case (s & t1 %%~ Const . Just, s & t2 %%~ Const . Just, s & t3 %%~ Const . Just) of
        (Const (Just a), Const (Just b), Const (Just c)) -> phantom $ fd (f a b c)
        _ -> pure s

ex ::  Fold (Either (String, String) ()) String
ex = liftT2 (++) (_Left . _1) (_Left . _2) `failing` (_Right . to show)

when :: Getter s Bool -> Traversal' s a -> Traversal' s a
when fld t fab s =
    if s ^. fld
        then t fab s
        else pure s
