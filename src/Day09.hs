module Day09 where

import IntCode
import Data.List.Split (splitOn)

example01 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
example02 = [1102,34915192,34915192,7,4,7,99,0]
example03 = [104,1125899906842624,99]

day09 :: IO ()
day09 = do
  dat <- splitOn "," <$> readFile "day09.txt"
  let
    m1 = createMachineFromList example01
    m2 = createMachineFromList example02
    m3 = createMachineFromList example03

    codes = fmap read dat :: [Int]
    m  = createMachineFromList codes
  print $ runMachineGenerator m1
  print $ runMachineGenerator m2
  print $ runMachineGenerator m3
  print $ runMachineGenerator m{ input=[1] }
  print $ runMachineGenerator m{ input=[2] }
