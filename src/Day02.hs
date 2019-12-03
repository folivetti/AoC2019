module Main where

import Data.List.Split (chunksOf, splitOn)

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
  day02 <- replaceCode 12 2 . fmap read . splitOn "," <$> readFile "day02.txt"

  print $ foldl applyRule examples2 codes
  print $ head $ compute day02
  print $ findOutput 19690720 day02
