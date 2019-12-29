module Plated where

import Control.Lens
import Data.Tree
import Data.Tree.Lens

tree :: Tree Int
tree =  Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]

-- >>> universe tree
-- [ Node 1 [ Node 2 [], Node 3 [ Node 4 [], Node 5 [] ] ]
-- , Node 2 []
-- , Node 3 [ Node 4 [] , Node 5 [] ]
-- , Node 4 []
-- , Node 5 []
-- ]

-- mostChildren :: Tree a -> [Int] -> Int
-- mostChildren (Node _ children) xs = maximum (length children : xs)

-- >>> para mostChildren tree
-- 2


-- >>> tree & transformM %~ (\t -> t & root .~ sum t)
-- Node 24 [Node 2 [], Node 12 [Node 4 [], Node 5, []]]
