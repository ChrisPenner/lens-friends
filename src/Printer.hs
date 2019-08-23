module Printer where

import Control.Lens
import Data.Tree
import Data.Tree.Lens

tree :: Tree Int
tree = Node 1 [Node 2 [Node 4 [Node 5 []]], Node 3 [Node 40 [], Node 50 []]]

values :: (a, Tree Int) -> [Int]
values = toListOf (_2 . deep (filtered (has $ root . only 3)) ... root)

-- printer :: (Int, Int) -> String
-- printer = i
