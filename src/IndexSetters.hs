{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module IndexSetters where

import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Text hiding (index, length)
import Data.String
import Text.RawString.QQ
import Data.Map as M
import Data.Bifunctor.Join

-- | Use the result of a getter over the current focus as the index
settingIndexOf :: Getter s i -> IndexedLens' i s s
settingIndexOf getter p s = indexed p (s ^. getter) s

-- | Set the index to some transformation of the input
-- Doesn't alter the focus
settingIndex :: (s -> i) -> IndexedLens' i s s
settingIndex getter p s = indexed p (getter s) s

-- Filter for elements which have at least one target for the given fold
-- Use with 'only' for equality checks
filteredHaving :: (Choice p, Applicative f) => Fold s a -> Optic' p f s s
filteredHaving f = filtered (has f)

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
