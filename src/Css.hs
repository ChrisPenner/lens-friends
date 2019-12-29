{-# LANGUAGE OverloadedStrings #-}
module Css where

-- import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as T
import System.IO.Unsafe
import Control.Lens hiding (children, element)
import qualified Control.Lens.Plated as P
import Text.Taggy.Lens
import HTML
-- import Text.Taggy

test :: Node
test = (unsafePerformIO  $ TL.readFile "src/test.html") ^?! html

-- sample = test ^.. deepOf nodeChildren (tag "pet") . cosmos . contents

---

-- element "h1 {}"

testElement = test ^.. anywhere (tag "h1")

-- testElements = test ^.. anywhere (tag "h1" `failing` tag "h2" `failing` tag "h3")
testElements = test ^.. anywhere (oneOf [tag "h1", tag "h2", tag "h3"])

testClass = test ^.. anywhere (hasClass "title")

testDescendent = test ^.. anywhere (tag "pet") . anywhere (tag "h2")

testAdjacent = test ^.. anywhere (tag "pet") . anywhere (tag "h2")
