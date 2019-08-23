module SYB where

import Control.Lens
import Control.Lens.Plated
import Data.Data.Lens
import Data.Tree
import Data.Tree.Lens
import Data.Data

tree :: Tree Int
-- tree = Node 1 [Node 2 [Node 4 [Node 5 []]], Node 3 [Node 40 [], Node 50 []]]
tree = Node 1 [Node 2 [Node 3 []]]

-- xs :: [[[Int]]]
-- xs = [[[1, 2, 3], [4, 5, 6]]]



-- How can we keep track of the index of this whole thing regardless of the filter
-- biplate . deepOf uniplate (taking 1 $ filtered p)

cata :: (Data a, Monad m) => LensLike' m a a
cata = transformMOf uniplate
