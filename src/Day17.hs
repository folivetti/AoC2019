module Day17 where

import IntCode
import Data.List (transpose, group, intercalate)
import Data.List.Split (splitOn)
import Data.Char
import qualified Data.Map.Strict as M

data Dir = UP | DOWN | LFT | RGT 

turnRight, turnLeft :: Dir -> Dir

turnRight UP   = RGT
turnRight DOWN = LFT
turnRight LFT  = UP
turnRight RGT  = DOWN

turnLeft UP   = LFT
turnLeft DOWN = RGT
turnLeft LFT  = DOWN
turnLeft RGT  = UP

findIntersections :: [String] -> [(Int, Int)]
findIntersections xss = [(i, j) | i <- [1..(n-2)]
                                , j <- [1..(m-2)]
                                , xss !! i !! j == '#'
                                , isCross i j]
  where
    n = length xss
    m = length (head xss)
    isCross i j = all (=='#') [(xss !! (i+1)) !! j
                              ,(xss !! (i-1)) !! j
                              ,(xss !! i    ) !! (j+1)
                              ,(xss !! i    ) !! (j-1 )
                              ]
    
countIntersections :: [String] -> Int
countIntersections xss = sum $ fmap (uncurry (*)) coords
  where
    coords = findIntersections xss

rowsWithScaff, colsWithScaff :: [String] -> M.Map Int [Int]
rowsWithScaff screen = M.fromList $ zip [0..] $ fmap f screen
  where
    f s = fmap fst $ filter ((/='.').snd) $ zip [0..] s

colsWithScaff = rowsWithScaff . transpose

stepsRight, stepsLeft, stepsUp, stepsDown :: (Int, Int) -> M.Map Int [Int] -> Int

stepsRight (x,y) rows = if null xs then 0 else head  xs
  where xs = steps (dropWhile (/=y) $ rows M.! x)
stepsLeft (x,y) rows  = if null xs then 0 else last xs
  where xs = steps (takeUntil (==y) $ rows M.! x)

steps :: [Int] -> [Int]
steps segs = fmap (\xs -> length xs - 1) . group $ zipWith (-) segs [0..]

takeUntil p [] = []
takeUntil p (x:xs) | p x = [x]
                   | otherwise = x : takeUntil p xs

stepsDown (x,y) = stepsRight (y,x) 
stepsUp   (x,y) = stepsLeft  (y,x) 

--seqCommands :: [String] -> [(Char, Int)]
seqCommands screen = seqCommands' (x,y) d0
  where
    rows = rowsWithScaff screen
    cols = colsWithScaff screen
    x    = fst $ head $ filter (any isRobot.snd) $ zip [0..] screen
    y    = fst $ head $ filter (isRobot.snd) $ zip [0..] (screen !! x)
    d0   = getDir (screen !! x !! y)
   
    seqCommands' c UP | sr==sl = []
                      |otherwise = if sr > sl
                        then ('R', sr) : seqCommands' (goRight c sr) RGT
                        else ('L', sl) : seqCommands' (goLeft  c sl) LFT
      where
        sr = stepsRight c rows
        sl = stepsLeft  c rows
    
    seqCommands' c DOWN | sr==sl = []
                        | otherwise = if sr > sl
                          then ('L', sr) : seqCommands' (goRight c sr) RGT
                          else ('R', sl) : seqCommands' (goLeft  c sl) LFT
      where
        sr = stepsRight c rows
        sl = stepsLeft  c rows

    seqCommands' c RGT | sd==su = [('R',sd)]
                       |otherwise = if sd > su
                         then ('R', sd) : seqCommands' (goDown c sd) DOWN
                         else ('L', su) : seqCommands' (goUp   c su) UP
      where
        sd = stepsDown c cols
        su = stepsUp   c cols

    seqCommands' c LFT | sd==su = []
                       |otherwise = if sd > su
                         then ('L', sd) : seqCommands' (goDown c sd) DOWN
                         else ('R', su) : seqCommands' (goUp   c su) UP
      where
        sd = stepsDown c cols
        su = stepsUp   c cols

goRight (x,y) dy = (x,y+dy)
goLeft  (x,y) dy = (x,y-dy)
goUp    (x,y) dx = (x-dx,y)
goDown  (x,y) dx = (x+dx,y)

getDir '^' = UP
getDir 'v' = DOWN
getDir '<' = LFT
getDir '>' = RGT

isRobot '^' = True
isRobot 'v' = True
isRobot '<' = True
isRobot '>' = True
isRobot  _  = False

instructionToStr (d, x) = d : ',' : show x
seqToStr = fmap instructionToStr 


a = "R,4,R,10,R,8,R,4\n"
b = "R,10,R,6,R,4\n"
c = "R,4,L,12,R,6,L,12\n"

cheat = "A,B,A,B,C,B,C,A,B,C\n"

day17 :: IO ()
day17 = do
  dat <- splitOn "," <$> readFile "day17.txt"
  let
    codes  = fmap read dat :: [Int]
    m      = createMachineFromList codes
    m'     = createMachineFromList (2 : tail codes)
    os     = runMachineGenerator m
    screen = init $ lines $ fmap chr os
    s      = seqCommands screen
    myInp  = fmap ord (cheat ++ a ++ b ++ c ++ "n\n")
  putStrLn $ fmap chr os
  print    $ countIntersections screen
  print    $ seqToStr s
  print    $ runMachineGenerator m'{ input=myInp }
