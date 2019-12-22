{-# LANGUAGE BangPatterns #-}
 
module Day22 where

import Data.List


type Mod = (Integer, Integer, Integer)

rev :: Integer -> Mod
rev n = (-1, -1, n)

cut, inc :: Integer -> Integer -> Mod
cut n k = (1, -(k `mod` n), n)
inc n k = (k `mod` n, 0, n)

applyMod :: Mod -> Integer -> Integer
applyMod (a,b,n) x = axb `mod` n
  where
    ax  = (a*x) `mod` n
    axb = (ax+b) `mod` n

solveMod :: Mod -> Integer -> Integer
solveMod (a,b,n) y = (yb*ra)  `mod` n
  where
    yb = (y-b) `mod` n
    ra = stimes (n-2) f a -- a^(n-2) `mod` n
    f x y = (x*y) `mod` n

composeMod :: Mod -> Mod -> Mod
composeMod (a,b,n) (x,y,m) = (ax, bxy, n)
  where
    ax  = (a*x) `mod` n
    bx  = (b*x) `mod` n
    bxy = (bx+y) `mod` n

--  | slow mode
dealNew :: [Int] -> [Int]
dealNew = reverse

cutN :: Int -> [Int] -> [Int]
cutN n xs | n >= 0    = r  ++ l
          | otherwise = cutN (length xs + n) xs
  where 
    (l,r) = splitAt n xs

incrementOf :: Int -> [Int] -> [Int]
incrementOf n xs = map snd $ sortOn fst shuf
  where
    idx  = 0 : go 0
    go x = let x' = (x+n) `rem` size
           in  x' : go x'
    size = length xs
    shuf = zip (take size idx) xs


strToFun :: String -> ([Int] ->  [Int])
strToFun css | "cut" `isPrefixOf` css       = cutN n
             | "deal into" `isPrefixOf` css = dealNew
             | "deal with" `isPrefixOf` css = incrementOf n
             | otherwise                    = error "No Pattern!"
  where
    n = (read.last.words) css

parseFun :: Integer -> String -> Mod
parseFun n css | "cut" `isPrefixOf` css       = cut n k
               | "deal into" `isPrefixOf` css = rev n
               | "deal with" `isPrefixOf` css = inc n k
               | otherwise                    = error "No Pattern!"
  where
    k = (read.last.words) css


applyInstructions fs xs = foldl (flip ($)) xs fs

-- | copiped from http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Semigroup.Internal.html#stimesDefault
stimes :: Integer -> (a -> a -> a) -> a -> a
stimes n op f
  | n <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
  | otherwise = evenF f n
  where
    evenF !x !y
      | even y = evenF (x `op` x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x `op` x) (y `quot` 2) x        -- See Note [Half of y - 1]
    g !x !y !z
      | even y = g (x `op` x) (y `quot` 2) z
      | y == 1 = x `op` z
      | otherwise = g (x `op` x) (y `quot` 2) (x `op` z) -- See Note [Half of y - 1]

day22 :: IO ()
day22 = do
  dat <- readFile "day22.txt"
  let
    is =  fmap (parseFun 10007) (lines dat)
    f  = foldl composeMod (1,0, 10007) is

    is' = fmap (parseFun 119315717514047) (lines dat)
    f'  = foldl composeMod (1,0,119315717514047) is'
    mf  = stimes 101741582076661 composeMod f' 
  print $ applyMod f 2019
  print $ solveMod f 1234
  print $ solveMod mf 2020
