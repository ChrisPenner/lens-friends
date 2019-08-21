{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zippers where

import Control.Zipper
import Control.Zipper.Internal
import Control.Lens
import Data.Foldable

-- Fill in blanks with Nothing
space :: [[Maybe Char]]
space =
    [ [Nothing,  Nothing,  Just '1', Nothing,  Nothing ]
    , [Nothing,  Just '2', Just '3', Just '4', Nothing ]
    , [Just '5', Just '6', Just '7', Just '8', Just '9']
    , [Nothing,  Just 'A', Just 'B', Just 'C', Nothing ]
    , [Nothing,  Nothing,  Just 'D', Nothing,  Nothing ]
    ]

-- A zipper into characters indexed by (Int, Int)
type Z = Zipper (Zipper Top Int [[Maybe Char]]) (Int, Int) Char
zSpace :: Zipper (Zipper Top Int [[Maybe Char]]) (Int, Int) Char
zSpace = zipper space & ifromWithin (traversed <.> traversed <. _Just)

-- Make a relative movement from the current position. Staying still on failure
moveBy :: (Int, Int) -> Z -> Z
moveBy (x, y) z =
    case focalPoint z of
        (x', y') -> tug (moveTo (x' + x, y' + y)) z

-- Movement ops
move :: Move -> Z -> Z
move U = moveBy (-1, 0)
move D = moveBy (1, 0)
move L = moveBy (0, -1)
move R = moveBy (0, 1)

data Move = U | D | L | R

runMoves :: [Move] -> Char
runMoves moves = foldl' (flip move) zSpace moves & view focus

