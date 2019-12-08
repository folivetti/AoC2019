module Day08 where

import Data.List.Split (chunksOf)
import Data.List
import Data.Char

example = "123456789012"

layers :: Int -> Int -> [a] -> [[a]]
layers w h = chunksOf (w*h)

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (==x)

countZeros, countOnes, countTwos :: String -> Int
countZeros = countElem '0'
countOnes = countElem '1'
countTwos = countElem '2'

minimumOn f = head . sortOn f

findLessZeros :: [String] -> String
findLessZeros xs = fst . minimumOn snd $ zip xs (fmap countZeros xs)

stack :: [String] -> String
stack (x:xs) = foldl compose x xs
  where
    compose = zipWith overlap
    overlap '2' p  = p
    overlap p _    = p

answer01 :: [String] -> Int
answer01 xs = countOnes zs * countTwos zs
  where zs = findLessZeros xs

answer02 :: [String] -> String
answer02 = fmap replaceChars . unlines . chunksOf 25 . stack
  where
    replaceChars '0' = ' '
    replaceChars '1' = '█'
    replaceChars c   = c

day08 :: IO ()
day08 = do
  dat <- init <$> readFile "day08.txt"
  let
    lays = layers 25 6 dat
  print $ answer01 lays
  putStrLn $ answer02 lays

