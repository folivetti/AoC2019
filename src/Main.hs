module Main where

import Data.List.Split (chunksOf, splitOn)

-- | Day 01
type Mass = Int
type Fuel = Int

howManyFuel :: Mass -> Fuel
howManyFuel m = m `div` 3 - 2

-- | repeatdly apply `howManyFuel` (iterate)
-- discard the mass (tail)
-- take all positive values (takeWhile)
-- return the sum
recursiveFuel :: Mass -> Fuel
recursiveFuel = sum . takeWhile (>0) . tail  . iterate howManyFuel

foldWithMap f op = foldr (\x y -> f x `op` y)

examples = [12, 14, 1969, 100756]

-- | Day 02
examples2 = [1,9,10,3,2,3,11,0,99,30,40,50]
codes     = chunksOf 4 examples2

applyRule :: [Int] -> [Int] -> [Int]
applyRule xs [1, i, j, k] =  take k xs ++ x' : drop (k+1) xs
  where x' = (xs!!i) + (xs!!j)
applyRule xs [2, i, j, k] =  take k xs ++ x' : drop (k+1) xs
  where x' = (xs!!i) * (xs!!j)
applyRule xs _ = xs

compute :: [Int] -> [Int]
compute xs = foldl applyRule xs (chunksOf 4 xs)

replaceCode :: Int -> Int -> [Int] -> [Int]
replaceCode noun verb (x:y:z:xs) = x : noun : verb : xs

findOutput :: Int -> [Int] -> (Int, Int)
findOutput x xs = (head . filter isX) ops
  where range = [0..99]
        ops   = [(n, v) | n <- range, v <- range]
        isX = (==x) . head . compute . (flip. uncurry) replaceCode xs


main :: IO ()
main = do
  day01 <- fmap read . lines <$> readFile "day01.txt"
  day02 <- replaceCode 12 2 . fmap read . splitOn "," <$> readFile "day02.txt"

  print $ howManyFuel   <$> examples
  print $ foldWithMap howManyFuel   (+) 0 day01
  
  print $ recursiveFuel <$> examples
  print $ foldWithMap recursiveFuel (+) 0 day01

  print $ foldl applyRule examples2 codes
  print $ head $ compute day02
  print $ findOutput 19690720 day02
