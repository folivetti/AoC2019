module Day16 where

import Data.Char (digitToInt)

test = [1,2,3,4,5,6,7,8]

pat = cycle [0, 1, 0, -1]

generatePattern cyc  = tail . concatMap (replicate cyc)

makePattern :: Int -> Int -> [Int] -> [Int]
makePattern n c initPat = init' ++ makePattern n (c+1) initPat
  where
    init'  = take n (generatePattern c initPat)

foldEvery n [] = []
foldEvery n xs = sum ixs :  foldEvery n txs
  where
    ixs = take n xs
    txs = drop n xs

takeLastDigit :: Int -> Int
takeLastDigit x = abs x `mod` 10 

-- | they should make more effort when expĺaining the problem :/
partb xs = partSum xs (sum xs)
  where
    partSum [] _ = []
    partSum (x:xs) tot = takeLastDigit tot : partSum xs (tot - x)

calcNum p xs = fmap takeLastDigit $ foldEvery n $ take (n*n) $ zipWith (*) p (cycle xs)
  where
    n = length xs

tenThousands = go 10000 
  where
    go 0 xs = []
    go n xs = xs ++ go (n-1) xs

day16 :: IO ()
day16 = do
  dat <- fmap digitToInt . filter (/='\n') <$> readFile "day16.txt"
  let
    n      = length dat
    p      = makePattern n 1 pat
    phases = iterate (calcNum p) dat 
    dat'   = concat (replicate 10000 dat)
    offset = read $ concatMap show $ take 7 dat'
    partB  = iterate partb (drop offset dat')
    phasesB = take 8 (partB !! 100)
  print $ concatMap show $ take 8 $ phases !! 100
  print phasesB
