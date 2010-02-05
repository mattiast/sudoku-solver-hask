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
kmoukari k ma = do
    nss <- nakedSubsets
    sequence_ [ eliminate ma ns | ns <- nss ]
    where
    -- En keksinyt tätä nimeä itse, vaan kyseessä on 
    -- "naked pair"-menetelmän yleistys kaikkiin osajoukkoihin.
    nakedSubsets = fmap (filter (\(_,_,l) -> blength l == k && l /= 0 && l /= 1022)) $ 
                    sequence [ l s u | u <- unitlist, s <- ksublists k u ]
                  where l s u = {-# SCC "or_fold" #-} do 
                                ls <- mapM (readArray ma) s
                                let ll = foldr1 (.|.) ls
                                return (u,s,ll)
    eliminate :: STUArray s Square Int16 -> ([Square],[Square],Int16) -> ST s ()
    eliminate ma (u,s,l) = {-# SCC "eliminate" #-}
			       sequence_ [ upd sq ma (.&. complement l) | sq <- u \\ s ] where
					 upd s m f = do {v <- readArray m s; writeArray m s (f v)}

moukari ma = do
    s_old <- smahisSum ma
    prog <- oper s_old ma [1,8,2,7,3,6,4,5] -- Tämä rivi teki ohjelmasta 10x nopeamman!
    if prog then moukari ma else return ()
    where
        oper s_old ma [] = return False  -- no progress
        oper s_old ma (k:ks) = do
            kmoukari k ma
            s_new <- smahisSum ma
            if s_old == s_new then oper s_old ma ks else return True

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

smahisSum ma = do
    es <- getElems ma
    return $ sum [ blength x | x <- es ]

mahisSum :: Mahis -> Int16
mahisSum = sum . map blength . elems

mahisNotRR   :: Mahis -> Bool
mahisNotRR m = foldl (\x y -> x && (y /= 0)) True (elems m)

mahisSaturate :: Mahis -> Mahis
mahisSaturate mah = runSTUArray $ do
    ma <- thaw mah
    moukari ma
    return ma
