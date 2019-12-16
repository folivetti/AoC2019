module Day04 where

import Data.List

toList :: Int -> [Int]
toList 0 = []
toList x = x `rem` 10 : toList (x `div` 10)

hasDouble :: [Int] -> Bool
hasDouble = (not.null) . filter (==2) . fmap length . group

neverDecreases :: [Int] -> Bool
neverDecreases []  = True
neverDecreases [x] = True
neverDecreases (x:y:xs) = (x >= y) && neverDecreases (y:xs)

valid :: Int -> Bool
valid x = hasDouble xs && neverDecreases xs
  where xs = toList x

tests = [111111, 223450, 123789, 112233, 123444, 111122]

range = [240920..789857]

day04 :: IO ()
day04 = do
  print $ fmap valid tests
  print $ (length . filter valid) range
