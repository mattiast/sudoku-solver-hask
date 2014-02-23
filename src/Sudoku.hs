{-# LANGUAGE PackageImports #-}
module Sudoku where
import Data.Char
import Control.Monad
import Control.Monad.ST
import "mtl" Control.Monad.List
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits
import Data.Int(Int16)
import Data.List
import Foreign.C
import Prelude

-- Cartesian product
cross           :: [a] -> [b] -> [(a,b)]
cross [] _      = []
cross (x:xs) ys = map (\z -> (x,z)) ys ++ cross xs ys

-- Power set
sublists        :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = map (x:) (sublists xs) ++ sublists xs

-- Subsets with k elements
ksublists 0 _ = [[]]
ksublists k [] = []
ksublists k (x:xs) = map (x:) (ksublists (k-1) xs) ++ ksublists k xs

-- how many ones in the binary expansion
blength :: Int16 -> Int
blength = popCount

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

-- Every square has a set of possible numbers
-- encoded in the bits 1..9
-- Other bits are zero.
type Mahis = UArray Square Int16

-- Find and eliminate naked subsets with k elements
kmoukari :: Int -> STUArray s Square Int16 -> ST s ()
kmoukari k ma = do
    void $ runListT nakedSubsets
    where
    -- A "naked subset" is a generalization of "naked pair".
    nakedSubsets = do
        unit <- ListT (return unitlist)
        sub <- ListT . return $ ksublists k unit
        ls <- lift $ mapM (readArray ma) sub
        -- By definition, a subset of size k is naked if there are exactly
        -- k bits that appear in those squares.
        let bits = foldl1' (.|.) ls
        True <- return $ blength bits == k
        lift $ eliminate ma (unit, sub, bits)
    -- Given a naked subset, remove their bits from all remaining squares
    -- in the same unit.
    eliminate :: STUArray s Square Int16 -> ([Square],[Square],Int16) -> ST s ()
    eliminate ma (unit, naked, bits) = {-# SCC "eliminate" #-}
                   sequence_ [ upd sq ma (.&. complement bits) | sq <- unit \\ naked ]
    upd sq m f = do v <- readArray m sq
                    writeArray m sq (f v)

-- Try to use kmoukari for different values of k. If it makes any progress,
-- start over from the beginning. If none of the k's gives any progress,
-- stop.
moukari :: STUArray s Square Int16 -> ST s ()
moukari ma = do
    s_old <- smahisSum ma
    let try (k:ks) = do
            kmoukari k ma
            s_new <- smahisSum ma
            if (s_new < s_old)
                then return True
                else try ks
        try [] = return False
    -- Changing the order from [1..9] made the program run 10x faster!
    progress <- try [1,8,2,7,3,6,4,5]
    when progress (moukari ma)

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

smahisSum :: STUArray s Square Int16 -> ST s Int
smahisSum ma = do
    es <- getElems ma
    return $ sum [ blength x | x <- es ]

mahisSum :: Mahis -> Int
mahisSum = sum . map blength . elems

mahisNotRR   :: Mahis -> Bool
mahisNotRR m = all (/= 0) (elems m)

mahisSaturate :: Mahis -> Mahis
mahisSaturate mah = runSTUArray $ do
    ma <- thaw mah
    moukari ma
    return ma
