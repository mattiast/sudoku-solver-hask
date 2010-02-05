module Sudoku where
import Char
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits
import Data.Int(Int16)
import Data.List
import Foreign.C
import List
import Prelude

-- listojen "karteesinen tulo" ja potenssijoukko
cross           :: [a] -> [b] -> [(a,b)]
cross [] _      = []
cross (x:xs) ys = map (\z -> (x,z)) ys ++ cross xs ys

sublists        :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = map (x:) (sublists xs) ++ sublists xs

ksublists 0 _ = [[]]
ksublists k [] = []
ksublists k (x:xs) = map (x:) (ksublists (k-1) xs) ++ ksublists k xs
--ksublist 0 0 n xs = []
--ksublist l k n (x:xs) | l < comb (n-1) (k-1) = x : ksublist l (k-1) (n-1) xs
--ksublist l k n (x:xs) = ksublist (l - comb (n-1) (k-1)) k (n-1) xs
--ksublists k xs = [ ksublist l k n xs | l <- [0..comb n k-1] ] where n = genericLength xs
--
--comb n 0 = 1
--comb n k = div ((n-k+1)*comb n (k-1)) k

-- kertoo, montako ykköstä luvun binääriesityksessä
foreign import ccall unsafe "blength.h blength" c_blength :: CUInt -> CUInt
blength :: Int16 -> Int16
blength = fromIntegral . c_blength . fromIntegral 
{- blength i = helper i 0 where
	helper 0 s = s
	helper i s = helper (shift i (-1)) (s+ (i.&.1)) -}

type Square = (Int,Char)

squares :: [Square] 
squares = cross [1..9] "abcdefghi"
unitlist :: [[Square]]
unitlist = rows ++ columns ++ boxes
	where
	rows = map (\x -> cross [x] "abcdefghi") [1..9]
	columns = map (\x -> cross [1..9] [x] ) "abcdefghi"
	boxes = liftM2 cross [[1..3],[4..6],[7..9]] ["abc","def","ghi"]

units s = filter (elem s) unitlist
peers s = filter (/=s) $ nub $ concat $ units s

-- Jokaisella ruudulla on mahdollisten numeroiden joukko 
-- koodattuna bitteihin 1..9
-- Muut bitit nollia
type Mahis = UArray Square Int16

--kmoukari :: Int -> Mahis -> Mahis
kmoukari k mah = runSTUArray $ do
	ma <- thaw mah
	sequence_ [ eliminate ma ns | ns <- nakedSubsets mah ]
	return ma
	where
	-- En keksinyt tätä nimeä itse, vaan kyseessä on 
	-- "naked pair"-menetelmän yleistys kaikkiin osajoukkoihin.
	nakedSubsets :: Mahis -> [([Square],[Square],Int16)]
	nakedSubsets m = {-# SCC "nakedSubsets" #-} do 
		 u <- unitlist
		 s <- ksublists k u
		 let l = {-# SCC "or_fold" #-} foldr1 (.|.) (map (m!) s)
		 True <- return $ (blength l == k) && l /= 0 && l /= 1022
                 return (u,s,l)
	eliminate ma (u,s,l) = {-# SCC "eliminate" #-}
			       sequence_ [ upd sq ma (.&. complement l) | sq <- u \\ s ] where
					 upd s m f = do {v <- readArray m s; writeArray m s (f v)}

moukari :: Mahis -> Maybe Mahis
moukari mah = let ms = map (flip kmoukari mah) [1,8,2,7,3,6,4,5]
              in find (\m ->mahisSum m < mahisSum mah) ms
        

mahisParse :: String -> Mahis
mahisParse string =  array ((1,'a'),(9,'i')) $ do
	(ln,l) <- zip [1..9] (lines string)
	True <- return $ l /= ""
	(a,d) <- zip "abcdefghi" l
	return ((ln,a), if d `elem` "123456789"
			        then bit (digitToInt d) else 1022)

mahisRender :: Mahis -> String
mahisRender m = do 
	s <- squares
	let p = m!s
	    pj = filter (testBit p) [1..9]
	    str [k] = show k
	    str _   = show 0
	str pj ++ (if snd s == 'i' then "\n" else "")

mahisSum m = foldl' (\x y -> x + blength y) 0 (elems m)

mahisNotRR   :: Mahis -> Bool
mahisNotRR m = foldl (\x y -> x && (y /= 0)) True (elems m)

mahisSaturate :: Mahis -> Mahis
mahisSaturate m = case moukari m of
                    Just m' -> mahisSaturate m'
                    Nothing -> m
