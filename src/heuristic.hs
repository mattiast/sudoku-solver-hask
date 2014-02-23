module Main where
import Sudoku
import Data.Tree
import Data.Bits
import Data.List
import Control.Arrow
import Data.Array.Unboxed

generateTree :: Mahis -> Tree Mahis
generateTree = unfoldTree ((id &&& guesses) . mahisSaturate) where
    guesses m 
        | mahisSum m == 81 = []
        | not (mahisNotRR m) = []
        | otherwise = let (sq,_) = minimumBy (\x y -> compare (snd x) (snd y)) $ 
                                           filter ((/=1) . snd) $ map (id *** blength) $ assocs m
                          mahikset = [ bit i | i <- [1..9], testBit (m!sq) i ]
                      in [ m // [(sq, v)] | v <- mahikset ]

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

