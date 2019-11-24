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
import qualified Data.Map as M

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


data Person =
    Person { _name :: String
           , _pet  :: Pet }
    deriving Show

data Pet =
    Pet { _petName :: String }
    deriving Show
makeLenses ''Person
makeLenses ''Pet

people :: [Person]
people = [Person "John" (Pet "Garfield"), Person "Calvin" (Pet "Hobbes")]

-- >>> people ^. folded . to (traverseOf (pet.petName) (join M.singleton))
-- fromList
--     [
--         ( "Garfield"
--         , Person
--             { _name = "John"
--             , _pet = Pet { _petName = "Garfield" }
--             }
--         )
--     ,
--         ( "Hobbes"
--         , Person
--             { _name = "Calvin"
--             , _pet = Pet { _petName = "Hobbes" }
--             }
--         )
--     ]
