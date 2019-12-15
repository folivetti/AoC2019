module Day11 where

import IntCode
import Data.List.Split (splitOn)
import Data.List (nub)
import qualified Data.Map.Strict as M

data Dir = UP | DOWN | LFT | RGT

type Coord = (Int, Int)
type Robot = (Int, Int, Dir)

left, right :: Robot -> Robot

left (x,y,UP)   = (x-1,y,LFT)
left (x,y,DOWN) = (x+1, y, RGT)
left (x,y,LFT)  = (x,y-1,DOWN)
left (x,y,RGT)  = (x,y+1,UP)

right (x,y,UP)   = (x+1,y,RGT)
right (x,y,DOWN) = (x-1, y, LFT)
right (x,y,LFT)  = (x,y+1,UP)
right (x,y,RGT)  = (x,y-1,DOWN)


getCoord :: Robot -> Coord
getCoord (x,y,_) = (x,y)

getInputs :: [Int] -> Robot -> M.Map Coord Int -> [Int]
getInputs [] _ _ = []
getInputs (color:dir:os) r state = M.findWithDefault 0 (getCoord r') state : getInputs os r' state'
  where
    r' = if dir == 0 then left r else right r
    state' = M.insert (getCoord r) color state

getOutputs :: Machine -> M.Map Coord Int -> [Int]
getOutputs m state = o
  where
    r = (0,0,UP)
    o = runMachineGenerator m{ input = 1 : getInputs o r state }

getSequence :: [Int] -> Robot -> [Coord]
getSequence [] _ = []
getSequence (color:dir:os) r = getCoord r : getSequence os r'
  where
    r' = if dir == 0 then left r else right r

draw :: [Int] -> M.Map Coord Int -> Robot -> M.Map Coord Int
draw [] m _ = m
draw (color:dir:os) m r = draw os m' r'
  where
    m' = M.insert (getCoord r) color m
    r' = if dir == 0 then left r else right r

drawingToList :: M.Map Coord Int -> [String]
drawingToList m = board
  where
    minX = minimum [fst k | (k,v) <- M.toList m]
    minY = minimum [snd k | (k,v) <- M.toList m]
    maxX = maximum [fst k | (k,v) <- M.toList m]
    maxY = maximum [snd k | (k,v) <- M.toList m]

    board =  [ [toChar $ M.findWithDefault 0 (x,y) m | x <- [minX..maxX]]
                                                     | y <- [minY..maxY]]

    toChar 0 = '.'
    toChar 1 = '#'

day11 :: IO ()
day11 = do
  dat <- splitOn "," <$> readFile "day11.txt"
  let

    codes = fmap read dat :: [Int]
    m  = createMachineFromList codes
    os = getOutputs m M.empty 
    drawing = draw os (M.singleton (0,0) 1) (0,0,UP)
  print $ length . nub $ getSequence os (0,0,UP)
  putStrLn $ unlines $ reverse $ drawingToList drawing 
