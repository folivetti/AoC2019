module Day25 where

import IntCode
import Data.List.Split (splitOn)
import Data.Char (chr, ord)
import Data.Maybe (fromJust, catMaybes, mapMaybe)
import Data.Map.Strict as M hiding (mapMaybe)
import Data.List (subsequences, isInfixOf)

data Door = North | South |  West | East
type Doors = [Door]

type Item  = String
type Items = [Item]
type Coord = (Int, Int)

(x,y) +: (z,w) = (x+z, y+w)

walk :: Door -> Coord -> Coord
walk North c = c +: ( 0, 1)
walk South c = c +: ( 0,-1)
walk West  c = c +: (-1, 0)
walk East  c = c +: ( 1, 0)

data State = S { pos :: Coord
               , doors :: Doors
               , items :: Items 
               , mac   :: Machine
               } 

instance Show Door where
  show North = "north\n"
  show South = "south\n"
  show East  = "east\n"
  show West  = "west\n"

readDoor :: String -> Maybe Door
readDoor "- north" = Just North
readDoor "- south" = Just South
readDoor "- west"  = Just West
readDoor "- east"  = Just East
readDoor _         = Nothing

readItem :: String -> Maybe Item
readItem ('-':' ':item) = Just item
readItem _              = Nothing

getDoors :: String -> Doors
getDoors = mapMaybe readDoor . lines

getItems :: String -> Items
getItems = mapMaybe readItem . dropWhile (/="Items here:") . lines

droppableItems = ["sand", "astrolabe", "mutex", "klein bottle", "semiconductor", "dehydrated water", "shell", "ornament"]

combinations = subsequences $ fmap (\s -> "drop " ++ s ++ "\n") droppableItems

lastStep :: Machine -> [String] -> IO ()
lastStep m ss = do
  let 
    ss'      = concat ss ++ "south\n"
    (m', os) = runUntilRead m{ input=fmap ord ss' } []
    output   = fmap chr os
  if "proceed" `isInfixOf` output
  then putStrLn output
  else putStr ""

playAdventure :: Machine -> [String] -> IO ()
playAdventure m [] = mapM_ (lastStep m) combinations

playAdventure m (s:stps) = do
  let (m', os) = runUntilRead m []
      text     = fmap chr os
      command  = fmap ord s -- <$> getLine
  --putStrLn text
  playAdventure m'{ input=command++[10] } stps


day25 = do
  dat   <- splitOn "," <$> readFile "day25.txt"
  steps <- lines <$> readFile "day25walk.txt"
  let
   code = fmap read dat
   m    = createMachineFromList code
  playAdventure m steps
  print $ length combinations
