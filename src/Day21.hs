module Day21 where

import IntCode
import Data.List.Split (splitOn)
import Data.Char

printOutput :: [Int] -> IO ()
printOutput [] = putStrLn "Error! No output!\n\n"
printOutput xs | last xs > 256  = print (last xs)
               | otherwise      = (putStrLn . fmap chr) xs

genProg :: [String] -> [Int]
genProg = fmap ord . unlines

progA  = [ "NOT T T"
         , "AND A T"
         , "AND B T"
         , "AND C T"
         , "NOT T J"
         , "AND D J"
         , "WALK"
         ]

progB  = [ "NOT T T"
         , "AND A T"
         , "AND B T"
         , "AND C T"
         , "NOT T T"
         , "NOT J J"
         , "AND E J"
         , "OR  H J"
         , "AND T J"
         , "AND D J"
         , "RUN"
         ]
day21 :: IO ()
day21 = do
  dat <- splitOn "," <$> readFile "day21.txt"
  let
    codes = fmap read dat
    m     = createMachineFromList codes
  printOutput $ runMachineGenerator m{ input=genProg progA }
  printOutput $ runMachineGenerator m{ input=genProg progB }
