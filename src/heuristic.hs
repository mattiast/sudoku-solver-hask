module Main where
import Sudoku
import List
import Data.Tree
import Data.Bits
import Data.List
import Control.Arrow
import Data.Array.Unboxed
import Text.XML.HaXml hiding ((!))
import Text.XML.HaXml.Pretty
--import Text.XML.HaXml.Combinators
import Text.PrettyPrint.HughesPJ hiding (cat)


generateTree :: Mahis -> Tree Mahis
generateTree = unfoldTree ((id &&& guesses) . mahisSaturate) where
	guesses m 
		| mahisSum m == 81 = []
		| not (mahisNotRR m) = []
		| otherwise = let (sq,_) = minimumBy (\x y -> compare (snd x) (snd y)) $ 
                                           filter ((/=1) . snd) $ map (id *** blength) $ assocs m
				  mahikset = [ bit i | i <- [1..9], testBit (m!sq) i ] in 
			      [ m // [(sq, v)] | v <- mahikset ]

treeSize (Node _ sf) = 1 + sum (map treeSize sf)
treeDepth (Node _ sf) = 1 + foldl' max 0 (map treeDepth sf)

renderHtml :: Mahis -> Int -> Int -> [Content i]
renderHtml m s d = cat [mkElemAttr "table"
                        [("cellpadding",literal "0"),("cellspacing",literal "0")] 
                        table,
                        literal ("Hakupuun koko: " ++ show s),
                        mkElem "br" [],
                        literal ("Hakupuun syvyys: " ++ show d)] undefined
    where table = map row $ lines (mahisRender m)
          row l = mkElem "tr" $ map datum l
          datum c = mkElem "td" [literal (return c)]

main :: IO ()
main = do
	mah <- return . mahisParse =<< getContents
	let t = generateTree mah
	    sols = filter (\m -> mahisNotRR m && mahisSum m == 81) $ concat $ levels t
	case sols of
		[] -> putStr "EI RATKAISUJA"
		[m] -> putStr $ 
                       (renderStyle (style{mode = OneLineMode}) . htmlprint) $ 
                       renderHtml m (treeSize t) (treeDepth t)
		ms -> do 
			putStr "USEITA RATKAISUJA"

