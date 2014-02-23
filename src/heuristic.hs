module Main where
import Sudoku
import Data.Tree
import Data.Bits
import Data.List
import Control.Arrow
import qualified Data.Vector.Unboxed as V

generateTree :: Mahis -> Tree Mahis
generateTree = unfoldTree ((id &&& guesses) . mahisSaturate) where
    guesses m 
        | mahisSum m == 81 = []
        | not (mahisNotRR m) = []
        | otherwise = let (_,sq) = V.minimum $ V.filter ((>1) . fst) $ V.map (blength *** id) $ V.zip m (V.enumFromN 0 81)
                          mahikset = [ bit i | i <- [1..9], testBit (m V.! sq) i ]
                      in [ m V.// [(sq, v)] | v <- mahikset ]

treeSize (Node _ sf) = 1 + sum (map treeSize sf)
treeDepth (Node _ sf) = 1 + foldl' max 0 (map treeDepth sf)

main :: IO ()
main = do
    mah <- return . mahisParse =<< getContents
    let t = generateTree mah
        sols = filter (\m -> mahisNotRR m && mahisSum m == 81) $ concat $ levels t
    case sols of
        [] -> putStr "NO SOLUTIONS"
        [m] -> putStr $ mahisRender m
        _ -> do 
            putStr "SEVERAL SOLUTIONS"

