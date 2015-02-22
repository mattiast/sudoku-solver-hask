module Main where
import Sudoku
import Search
import qualified Data.Foldable as F

main :: IO ()
main = do
    mah <- return . mahisParse =<< getContents
    let tree = generateTree mah
        sols = F.find (\m -> mahisNotRR m && mahisSum m == 81) tree
    case sols of
        Nothing -> putStr "NO SOLUTIONS"
        Just m -> putStr $ mahisRender m

