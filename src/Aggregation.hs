{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Aggregation where

import Text.RawString.QQ
import Data.ByteString
import Data.Aeson.Lens
import Data.Aeson
import Control.Lens
import qualified Data.Text as T
import Data.Function

myData :: ByteString
myData = [r|
{
  "Oct": {
    "11": {
      "pushups": 10
    },
    "12": {
      "pushups": 15
    }
  }
}
|]

(.<>) = icompose ((<>) `on` (:[]))

pushups :: [([ T.Text ], Integer)]
pushups = myData ^@.. ((members .<> members) <. (key "pushups" . _Integer))

-- >>> pushups
-- [(["Oct","12"],15),(["Oct","11"],10)]

-- >>> pushups
-- [(("Oct","12"),15),(("Oct","11"),10)]
