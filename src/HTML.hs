{-# LANGUAGE OverloadedStrings #-}
module HTML where

-- import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as T
import System.IO.Unsafe
import Control.Lens hiding (children, element)
import qualified Control.Lens.Plated as P
import Text.Taggy.Lens
-- import Text.Taggy

nodeChildren :: Traversal' Node Node
nodeChildren = P.plate

tag :: T.Text -> Traversal' Node Node
tag t = filteredBy (element . name . only t)

-- test :: Node
-- test = (unsafePerformIO  $ TL.readFile "src/test.html") ^?! html

-- sample = test ^.. deepOf nodeChildren (tag "pet") . cosmos . contents

anywhere :: (Conjoined p, Applicative f) => Traversing p f Node Node a b -> Over p f Node Node a b
anywhere t = deep t

oneOf :: (Conjoined p, Applicative f) => [Traversing p f s s a b] -> Over p f s s a b
oneOf (t : ts) = t `failing` oneOf ts
oneOf [] = ignored

hasClass :: T.Text -> Traversal' Node Node
hasClass cls = attrMatches "class" (T.isInfixOf cls)

hasId :: T.Text -> Traversal' Node Node
hasId = attrIs "id"

attrMatches :: T.Text -> (T.Text -> Bool) -> Traversal' Node Node
attrMatches attrName p = filteredBy (element . attr attrName . _Just . filtered p)

attrIs :: T.Text -> T.Text -> Traversal' Node Node
attrIs attrName val = attrMatches attrName (== val)

-- Selects children of current node with matching siblings
hasSibling :: Traversal' Node a -> Traversal' Node b -> Traversal' Node b
hasSibling ref selection =
