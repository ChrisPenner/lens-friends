module Prisms where

import Control.Lens

data Err = Err String
    deriving Show

errHandler :: Either Err a -> IO a
errHandler = either (fail . show) return

stringHandler :: Either String a -> IO a
stringHandler = errHandler & outside _Left . argument %~ Err

-- >>> stringHandler (Left "whoops")
-- *** Exception: user error (Err "whoops")

stringHandlerWithMsg :: Either String a -> IO a
stringHandlerWithMsg =
    stringHandler & outside _Left . mapped %~ (putStrLn "Failing!" >>)

-- >>> stringHandlerWithMsg (Left "whoops")
-- Failing!
-- *** Exception: user error (Err "whoops")


safeTail :: [a] -> [a]
safeTail = tail & outside _Empty . mapped .~ []

-- >>> safeTail []
-- []
-- >>> safeTail [1, 2, 3]
-- [2,3]

-- >>> tail []
-- *** Exception: Prelude.tail: empty list

