{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Sudoku(
             Mahis,
             mahisSum,
             mahisSaturate,
             mahisParse,
             mahisParseLineDot,
             mahisRender,
             mahisNotRR,
             blength
) where
import Data.Char
import Data.Foldable(for_)
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.Bits
import Data.Int(Int16)
import Data.List((\\), foldl1')
import Prelude

-- Cartesian product
cross           :: [a] -> [b] -> [(a,b)]
cross [] _      = []
cross (x:xs) ys = map (\z -> (x,z)) ys ++ cross xs ys

-- Subsets with k elements
ksublists :: Int -> [a] -> [[a]]
ksublists 0 _ = [[]]
ksublists _ [] = []
ksublists k (x:xs) = map (x:) (ksublists (k-1) xs) ++ ksublists k xs

-- how many ones in the binary expansion
blength :: Int16 -> Int
blength = popCount

type Square = Int

squares :: [Square] 
squares = [0..80]
unitlist :: [[Square]]
unitlist = map (map squareNumber) $ rows ++ columns ++ boxes
    where
    squareNumber (i,j) = 9*(i-1) + (j-1) :: Int
    rows = map (\x -> cross [x] [1..9]) [1..9]
    columns = map (\x -> cross [1..9] [x] ) [1..9]
    boxes = liftM2 cross [[1..3],[4..6],[7..9]] [[1..3],[4..6],[7..9]]

-- Every square has a set of possible numbers
-- encoded in the bits 1..9
-- Other bits are zero.
type Mahis = V.Vector Int16

-- Find and eliminate naked subsets with k elements
kmoukari :: forall s. Int -> VM.STVector s Int16 -> ST s ()
kmoukari k ma = nakedSubsets
    where
    -- A "naked subset" is a generalization of "naked pair".
    nakedSubsets :: ST s ()
    nakedSubsets = for_ unitlist $ \unit -> do
                       for_ (ksublists k unit) $ \sub -> do
                           ls <- mapM (VM.read ma) sub
                           -- By definition, a subset of size k is naked if there are exactly
                           -- k bits that appear in those squares.
                           let bits = foldl1' (.|.) ls
                           when (blength bits == k) $
                               eliminate (unit, sub, bits)
    -- Given a naked subset, remove their bits from all remaining squares
    -- in the same unit.
    eliminate :: ([Square],[Square],Int16) -> ST s ()
    eliminate (unit, naked, bits) = {-# SCC "eliminate" #-}
                   sequence_ [ upd sq ma (.&. complement bits) | sq <- unit \\ naked ]
    upd sq m f = do v <- VM.read m sq
                    VM.write m sq (f v)

-- Try to use kmoukari for different values of k. If it makes any progress,
-- start over from the beginning. If none of the k's gives any progress,
-- stop.
moukari :: VM.STVector s Int16 -> ST s ()
moukari ma = do
    s_old <- smahisSum ma
    let try (k:ks) = do
            kmoukari k ma
            s_new <- smahisSum ma
            if s_new < s_old
                then return True
                else try ks
        try [] = return False
    -- Changing the order from [1..9] made the program run 10x faster!
    progress <- try [1,8,2,7,3,6,4,5]
    when progress (moukari ma)

mahisParse :: String -> Mahis
mahisParse string =  V.fromListN 81 $ map bitti $ concat $ lines string where
    bitti d = if d `elem` "123456789"
                  then bit (digitToInt d) 
                  else 1022

mahisParseLineDot :: String -> Maybe Mahis
mahisParseLineDot line = V.fromListN 81 <$> mapM bitti line where
    bitti '.' = Just 1022
    bitti d | d >= '1' && d <= '9' = Just $ bit (digitToInt d)
    bitti _ = Nothing

mahisRender :: Mahis -> String
mahisRender m = do 
    s <- squares
    let p = m V.! s
        pj = filter (testBit p) [1..9]
        str [k] = show k
        str _   = "0"
    str pj ++ (if mod s 9 == 8 then "\n" else "")

smahisSum :: VM.STVector s Int16 -> ST s Int
smahisSum ma = do
    es <- mapM (VM.read ma) [0..80]
    return $ sum [ blength x | x <- es ]

mahisSum :: Mahis -> Int
mahisSum = V.sum . V.map blength

mahisNotRR   :: Mahis -> Bool
mahisNotRR m = V.all (/= 0) m

mahisSaturate :: Mahis -> Mahis
mahisSaturate mah = V.create $ do
    ma <- V.thaw mah
    moukari ma
    return ma
