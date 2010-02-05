module Main where
import Sudoku

main :: IO ()
main = do
	mah <- return . mahisSaturate . mahisParse =<< getContents
	if mahisNotRR mah
		then putStr $ mahisRender mah
		else putStr "RISTIRIITAINEN ASETELMA" 
