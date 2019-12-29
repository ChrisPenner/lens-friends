module Zippering where

import Control.Lens

zipperedList :: IndexedTraversal ([a], [a]) [a] [b] a b
zipperedList f xs = traverse (uncurry $ indexed f) (imap splitter xs)
  where
    splitter n x =
        case splitAt n xs of
            (before, after) -> ((before, drop 1 after), x)

zippered :: Traversable f => IndexedTraversal ([a], [a]) (f a) (f b) a b
zippered = zipperedOf traversed

zipperedOf :: Traversal s t a b -> IndexedTraversal ([a], [a]) s t a b
zipperedOf t = unsafePartsOf t . zipperedList


-- zipperingOf :: Lens s t a b -> IndexedTraversal ([a], [a]) [s] [t] a b
-- zipperingOf selection f xs = traverse (uncurry $ indexed f) (imap splitter xs)
--   where
--     splitter n x =
--         case splitAt n xs of
--             (before, after) -> ((before, drop 1 after), x)
