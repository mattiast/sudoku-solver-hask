module Main where

import qualified Data.Foldable as F
import Search
import Sudoku

main :: IO ()
main = do
  mah <- mahisParse <$> getContents
  let tree = generateTree mah
      sols = F.find (\m -> mahisNotRR m && mahisSum m == 81) tree
  case sols of
    Nothing -> putStr "NO SOLUTIONS"
    Just m -> putStr $ mahisRender m
