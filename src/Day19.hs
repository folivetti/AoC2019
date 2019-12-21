module Day19 where

import IntCode
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

checkArea :: Machine -> Int
checkArea m = sum $ fmap (\is -> head $ runMachineGenerator m{ input=is }) iss
  where
    iss = [[i,j] | i <- [0..49], j <- [0..49]]


getFittingArea :: Machine -> Int -> Int -> Int -> (Int, Int)
getFittingArea m x0 y0 dx = head $ filter isSquare [(x,y) | x <- [x0-dx .. x0+dx], y <- [y0-dx .. y0 + dx]]
  where
    isSquare (x,y) = hasBeam m [x,y] && hasBeam m [x+99,y-99]

findFstX m y  = head [x | x <- [0..], hasBeam m [x,y]]
findLastX m y = last $ takeWhile (\x -> hasBeam m [x,y]) [findFstX m y ..]
hasBeam  m xs = runMachineGenerator m{ input=xs } == [1]

findSlope :: Machine -> (Machine -> Int -> Int) -> Double
findSlope m f  = fromIntegral y / fromIntegral x
  where
    y = 10000
    x = f m y

findFstSlope m = findSlope m findFstX
findLastSlope m = findSlope m findLastX

day19 :: IO ()
day19 = do
  dat <- splitOn "," <$> readFile "day19.txt"
  let
    codes = fmap read dat
    m     = createMachineFromList codes
    os    = runMachineGenerator m{ input=[0,0] }
    b0    = findFstSlope m    
    b1    = findLastSlope m
    x'    = (99*b1 + 99)/(b0-b1)
    x     = floor x'
    y     = floor (b0*x' - 99)
    (x0,y0) = getFittingArea m x (y+100) 10
  print $ checkArea m
  print (x,y)
  print $ findLastX m (y-99)
  print $ x0*10000 + (y0-99)
