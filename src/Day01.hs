module Day01 where

import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)

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

day01 :: IO ()
day01 = do
  day01 <- fmap read . lines <$> readFile "day01.txt"

  print $ howManyFuel   <$> examples
  print $ foldWithMap howManyFuel   (+) 0 day01
  
  print $ recursiveFuel <$> examples
  print $ foldWithMap recursiveFuel (+) 0 day01

