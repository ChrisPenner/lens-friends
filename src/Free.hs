{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Free where

import qualified Control.Category as C
import Control.Lens
import Data.Aeson.Lens

import Control.Category.Free
import qualified Data.Text as T
import Data.Aeson

data JPath f a b where
    Key :: T.Text -> JPath f (Value -> f Value) (Value -> f Value)
    Ind :: Int -> JPath f (Value -> f Value) (Value -> f Value)
    Txt :: JPath f (T.Text -> f T.Text) (Value -> f Value)
    Dub :: JPath f (Double -> f Double) (Value -> f Value)


cat :: f a b -> Cat f a b
cat fab = fab :.: Id

key' :: T.Text -> Cat (JPath f) (Value -> f Value) (Value -> f Value)
key' = cat . Key

ind' :: Int -> Cat (JPath f) (Value -> f Value) (Value -> f Value)
ind' = cat . Ind

_Txt :: Cat (JPath f) (T.Text -> f T.Text) (Value -> f Value)
_Txt = cat Txt

_Dub :: Cat (JPath f) (Double -> f Double) (Value -> f Value)
_Dub = cat Dub

userName :: Cat (JPath f) (T.Text -> f T.Text) (Value -> f Value)
userName = key' "name" C.<<< _Txt

toLens :: forall f s a. Applicative f => Cat (JPath f) (a -> f a) (s -> f s) -> LensLike' f s a
toLens c = runC (toC c) go
  where
    go :: forall x y. JPath f x y -> x -> y
    go Txt = _String
    go Dub = _Number . iso (fromRational . toRational) (fromRational . toRational)
    go (Key k) = key k
    go (Ind i) = nth i


-- showPath :: Cat (JPath f) (a -> f a) (s -> f s) -> (String -> String)
-- showPath c = runC (toC c) _go
--   where
--     go
