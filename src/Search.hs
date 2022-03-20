module Search where

import Control.Arrow
import Data.Bits
import Data.List
import Data.Tree
import qualified Data.Vector.Unboxed as V
import Sudoku

generateTree :: Mahis -> Tree Mahis
generateTree = unfoldTree ((id &&& guesses) . mahisSaturate)
  where
    guesses m
      | mahisSum m == 81 = []
      | not (mahisNotRR m) = []
      | otherwise =
        let (_, sq) = V.minimum $ V.filter ((> 1) . fst) $ V.map (first blength) $ V.zip m (V.enumFromN 0 81)
            mahikset = [bit i | i <- [1 .. 9], testBit (m V.! sq) i]
         in [m V.// [(sq, v)] | v <- mahikset]

treeSize :: Tree a -> Int
treeSize (Node _ sf) = 1 + sum (map treeSize sf)

treeDepth :: Tree a -> Int
treeDepth (Node _ sf) = 1 + foldl' max 0 (map treeDepth sf)
