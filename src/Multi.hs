{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Multi where

import Control.Lens ((^.), makeFieldsNoPrefix)
import Prelude hiding (id)

data Foo = Foo { _id :: Int
               , _name :: String
               }
-- $(makeFieldsNoPrefix ''Foo)

data Bar
  = Bar1 { _id :: Int
         , _name :: String
         }
  | Bar2 { _name :: String }
-- $(makeFieldsNoPrefix ''Bar)


-- a = Foo 1 "name" ^. name -- compiles fine

-- b = Foo 2 "blah" ^. id -- doesnt compile
{-
    • No instance for (Monoid Int) arising from a use of ‘id’
    • In the second argument of ‘(^.)’, namely ‘id’
      In the expression: (undefined :: Foo) ^. id
      In an equation for ‘b’: b = (undefined :: Foo) ^. id
-}
