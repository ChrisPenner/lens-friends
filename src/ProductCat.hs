{-# LANGUAGE OverloadedStrings #-}
module ProductCat where

import qualified Data.Text as T
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Control.Arrow ((***))

type PLens s t a b = forall f. Applicative f => (String, a -> f b) -> (String, s -> f t)
type PLens' s a = PLens s s a a

key' :: T.Text -> PLens' Value Value
key' k = (<> ("." <> T.unpack k)) *** key k

nth' :: Int -> PLens' Value Value
nth' n = (<> ("[" <> show n <> "]")) *** nth n

_txt :: PLens' Value T.Text
_txt = id *** _String

path :: PLens' Value T.Text
path = nth' 0 . key' "name" . _txt

toLens :: PLens s t a b -> Traversal s t a b
toLens l f = snd $ l ("", f)

toPath :: PLens s t a b -> String
toPath l = fst $ l ("", const (Const ()))

obj :: Value
obj = ("[{\"name\": \"hi\"}]" :: T.Text) ^?! _JSON

testLens :: Maybe T.Text
testLens = obj ^? toLens path

-- >>> testLens
-- Just "hi"

testPath :: String
testPath = toPath path

-- >>> testPath
-- ".name[0]"

