{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Dependent where

import Control.Lens

-- Allows you to build a lens using context from the currently selected value
-- This lens is unsound when `a` overlaps with state used by the lens constructor you pass in.
unsoundIntrospecting :: (s -> LensLike' f s a) -> LensLike' f s a
unsoundIntrospecting mk f s = (mk s) f s

-- A sound version of introspecting which enforces separation between the state under focus
-- and the state used to construct the next lens.
introspecting :: Functor f => (k -> LensLike' f s a) -> LensLike' f (k, s) a
introspecting mk f (k, s) =
    let lns = mk k
     in (k,) <$> (lns f s)

data AccountType
    = Chequing
    | Savings
    deriving (Show, Eq)

data Account =
    Account
    { _primaryAccnt :: AccountType
    , _chequing     :: [Int]
    , _savings      :: [Int]
    } deriving (Show, Eq)
makeLenses ''Account

myAccount :: Account
myAccount = Account Chequing [1, 2, 3] [10, 20, 30]

primaryTransactions :: Traversal' Account Int
primaryTransactions = unsoundIntrospecting picker
  where
    picker (Account Chequing chq _)  = chequing . traversed
    picker (Account Savings _ svgs)  = savings . traversed

-- λ> myAccount ^.. primaryTransactions
-- [1,2,3]

-- λ> (myAccount & primaryAccnt .~ Savings) ^.. primaryTransactions
-- [10,20,30]

-- λ> (1, ["one", "two", "three", "four"]) ^. picker
-- "two"
-- λ> (2, ["one", "two", "three", "four"]) ^. picker
-- "three"
-- λ> (2, ["one", "two", "three", "four"]) & picker .~ "yo"
-- (2,["one","two","yo","four"])
