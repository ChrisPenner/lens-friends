{-# LANGUAGE OverloadedStrings #-}
module TextSpan where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.RawString.QQ (r)
import qualified Data.List as List

type Line = Int
type Col = Int


txt :: T.Text
txt = [r|12345678
middle
12345678|]

textSpan :: (Line, Col) -- ^ Start point (inclusive)
         -> (Line, Col) -- ^ End point (exclusive)
         -> Traversal' T.Text T.Text
textSpan (startLine, startCol) (endLine, endCol) =
    traverseOf ( iso T.lines T.unlines
               . overRange startLine endLine
               . restrictingColumns startCol endCol)
               . iso T.unlines T.lines

restrictingColumns :: Int -> Int -> Traversal' [T.Text]  [T.Text]
restrictingColumns startCol endCol f [] = f []
restrictingColumns startCol endCol f [x] =
    let (prefix, remainder) = T.splitAt startCol x
        (remainder', suffix) = T.splitAt (endCol - startCol) remainder
     in (\middle -> middle & _head %~ (prefix <>) & _last <>~ suffix) <$> f [remainder']
restrictingColumns startCol endCol f (firstLine : remainder) =
    case unsnoc remainder of
        Nothing -> pure []
        Just (middleLines, lastLine) ->
            let (prefix, firstRemainder) = T.splitAt startCol firstLine
                (lastRemainder, suffix) = T.splitAt endCol lastLine
                in (\middle -> middle & _head %~ (prefix <>) & _last <>~ suffix) <$> f (firstRemainder : (middleLines `snoc` lastRemainder))

overRange :: Int -> Int -> Traversal' [a] [a]
overRange start end f xs = do
    let (prefix, remainder) = List.splitAt start xs
        (remainder', suffix) = List.splitAt ((end - start) + 1) remainder
     in (\xs -> prefix <> xs <> suffix) <$> (f remainder')




-- simpleSpan :: (Line, Col) -- ^ Start point (inclusive)
--            -> (Line, Col) -- ^ End point (exclusive)
--            -> Traversal' T.Text T.Text
-- simpleSpan (startLine, startCol) (endLine, endCol) =
--     iso T.lines T.unlines
--     . partsOf
--     ( traversed
--     . indices (\i -> i >= startCol && i < endCol)
--     )
