module Main where

import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (mapMaybe)

-- | Day 03
data Wire = R Int | L Int | D Int | U Int
  deriving (Show)

type Coord = (Int, Int)

readWire ('R':cs) = R (read cs)
readWire ('L':cs) = L (read cs)
readWire ('D':cs) = D (read cs)
readWire ('U':cs) = U (read cs)

str2wires :: String -> [Wire]
str2wires cs = readWire <$> splitOn "," cs

-- Examples
wire1 = str2wires "R8,U5,L5,D3"
wire2 = str2wires "U7,R6,D4,L4"

wire3 = str2wires  "R75,D30,R83,U83,L12,D49,R71,U7,L72"
wire4 = str2wires  "U62,R66,U55,R34,D71,R55,D58,R83"

wire5 = str2wires  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
wire6 = str2wires  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

-- Make the sequence
wire2lines :: [Wire] -> [(Coord, Coord)]
wire2lines = from (0,0)
  where
    from _ [] = []
    from (x,y) (R z:ds) = ((x,y), (x+z,y)) : from (x+z,y) ds
    from (x,y) (L z:ds) = ((x,y), (x-z,y)) : from (x-z,y) ds
    from (x,y) (U z:ds) = ((x,y), (x,y+z)) : from (x,y+z) ds
    from (x,y) (D z:ds) = ((x,y), (x,y-z)) : from (x,y-z) ds


intersection :: (Coord, Coord) -> (Coord, Coord) -> Maybe Coord
intersection ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) 
  | x1==x2 && x3==x4 = Nothing
  | y1==y2 && y3==y4 = Nothing
  | x1==x2           = if   within x1 x3 x4 && within y3 y1 y2 
                       then Just (x1,y3) 
                       else Nothing
  | y1==y2           = if   within x3 x1 x2 && within y1 y3 y4
                       then Just (x3, y1) 
                       else Nothing
    where within x x1 x2 = (x >= min x1 x2) && (x <= max x1 x2)
             
findIntersections :: [(Coord, Coord)] -> [(Coord, Coord)] -> [Coord]
findIntersections ws1 ws2 = filter (/=(0,0)) boundedIntersects
  where boundedIntersects = mapMaybe (uncurry intersection) [(w1,w2) | w1 <- ws1, w2 <- ws2]

manhattan :: [Coord] -> [Int]
manhattan = map (\(x,y) -> abs x + abs y)

minManhattan :: [Coord] -> Int
minManhattan = minimum . manhattan

costWireCoord :: [Wire] -> Coord -> Int
costWireCoord ws (x,y) = go ws (0,0)
  where
    go [] (x', y') = 0
    go (R l:ws') (x', y') | y' == y && x' < x && x'+l > x = x - x'
                          |otherwise            = l + go ws' (x'+l, y')
    go (L l:ws') (x', y') | y' == y && x' > x && x'-l < x = x'-x
                          |otherwise            = l + go ws' (x'-l, y')
    go (U l:ws') (x', y') | x' == x && y' < y && y'+l > y = y - y'
                          |otherwise            = l + go ws' (x', y'+l)
    go (D l:ws') (x', y') | x' == x && y' > y && y'-l < y = y' - y
                          |otherwise            = l + go ws' (x', y'-l)

totCostWireCoord :: [Wire] -> [Wire] -> [Coord] -> [Int]
totCostWireCoord ws1 ws2 cs = zipWith (+) (costWireCoord ws1 <$> cs) (costWireCoord ws2 <$> cs)

main :: IO ()
main = do
  day03 <- fmap (fmap readWire . splitOn ",") . lines <$> readFile "day03.txt"

  let ws1 = day03 !! 0 --wire2lines wire5
      ws2 = day03 !! 1 -- wire2lines wire6
      ls1 = wire2lines ws1
      ls2 = wire2lines ws2
      is  = findIntersections ls1 ls2
  print $ minManhattan is
  print $ minimum (totCostWireCoord ws1 ws2 is)
