{-# LANGUAGE PartialTypeSignatures #-}
module Zippers where

import Control.Zipper
import Control.Zipper.Internal
import Control.Lens
import Data.Maybe
import Control.Monad

j = Just
b = Nothing

pad :: [[Maybe Char]]
pad =
    [ [b, b, j '1', b, b]
    , [b, j '2', j '3', j '4', b]
    , [j '5', j '6', j '7', j '8', j '9']
    , [b, j 'A', j 'B', j 'C', b]
    , [b, b, j 'D', b, b]
    ]

zpad :: Inside
zpad = fromJust $
    pure (zipper pad)
    >>= within traversed
    >>= rightward
    >>= rightward
    >>= within traversed
    >>= rightward
    >>= rightward

check z = if isJust (view focus z)
             then Just z
             else Nothing

type Inside = (Zipper
             (Zipper (Zipper Top Int [[Maybe Char]]) Int [Maybe Char])
             Int
             (Maybe Char))

moveDown :: Inside -> Inside
moveDown = tug moveDown'

moveUp :: Inside -> Inside
moveUp = tug moveUp'

moveDown' :: Inside -> Maybe Inside
moveDown' z = rightward (upward z) >>= within traversed >>= moveTo saved >>= check
    where
      saved = focalPoint z

moveUp' :: Inside -> Maybe Inside
moveUp' z = leftward (upward z) >>= within traversed >>= moveTo saved >>= check
    where
      saved = focalPoint z

moveLeft :: Inside -> Inside
moveLeft = tug (leftward >=> check)

moveRight :: Inside -> Inside
moveRight = tug (rightward >=> check)

-- type Outside = undefined

zpad2 :: Zipper (Zipper Top Int [[Maybe Char]]) (Int, Int) Char
zpad2 = fromJust $ zipper pad & iwithin (traversed <.> traversed <. _Just)

-- thing :: _
-- thing = zipper ["one", "two", "three"]
