module Day24 where

import qualified Data.Set as S
import qualified Data.Map.Strict as M 

type Coord = (Int, Int, Int)
data Life  = Alive | Dead
              deriving (Eq, Ord)

type Grid = M.Map Coord Life

adjacents = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0)]

(a,b,c) +: (x,y,z) = (a+x, b+y, c+z)

instance Show Life where
  show Alive = "#"
  show Dead  = "."

(!&) :: Grid -> Coord -> Life
m !& c = M.findWithDefault Dead c m

displayGrid :: Int -> Grid -> String
displayGrid lvl m = unlines rows
  where
    rows   = fmap (concatMap (show.(m !&))) coords
    coords = [[(i,j,lvl) | j <- [0..size-1]] | i <- [0..size-1]]

rule, ruleA :: Grid -> Coord -> Life

ruleA m s | cell == Alive  = if   liveNeigh == 1
                            then Alive
                            else Dead
         | otherwise      = if   liveNeigh == 1 || liveNeigh == 2
                            then Alive
                            else Dead
  where
    cell      = m !& s
    liveNeigh = length (filter (==Alive) neighbors)
    neighbors = map (m !&) [s +: c | c <- adjacents]

rule m s | cell == Alive  = if   liveNeigh == 1
                            then Alive
                            else Dead
         | otherwise      = if   liveNeigh == 1 || liveNeigh == 2
                            then Alive
                            else Dead
  where
    cell      = m !& s
    liveNeigh = length (filter (==Alive) neighbors)
    neighbors = map (m !&) adj
    adj       = case s of
                  -- neighbors of (2,2,z)
                  (1,2,z) -> [(0,2,z), (1,1,z), (1,3,z)] ++ [(0,y,z+1)| y <- [0..size-1]]
                  (2,1,z) -> [(1,1,z), (3,1,z), (2,0,z)] ++ [(x,0,z+1)| x <- [0..size-1]]
                  (2,3,z) -> [(1,3,z), (3,3,z), (2,4,z)] ++ [(x,size-1,z+1)| x <- [0..size-1]]
                  (3,2,z) -> [(3,1,z), (3,3,z), (4,2,z)] ++ [(size-1,y,z+1)| y <- [0..size-1]]
                 
                  --  neighbors of outer level
                  (0,0,z) -> [(1,2,z-1), (2,1,z-1), (0,1,z), (1,0,z)]
                  (0,4,z) -> [(1,2,z-1), (2,3,z-1), (0,3,z), (1,4,z)]
                  (4,0,z) -> [(2,1,z-1), (3,2,z-1), (3,0,z), (4,1,z)]
                  (4,4,z) -> [(2,3,z-1), (3,2,z-1), (3,4,z), (4,3,z)]

                  (0,y,z) -> [(1,2,z-1), (0,y-1,z), (0,y+1,z), (1,y,z)]
                  (x,0,z) -> [(2,1,z-1), (x-1,0,z), (x+1,0,z), (x,1,z)]
                  (4,y,z) -> [(3,2,z-1), (4,y-1,z), (4,y+1,z), (3,y,z)]
                  (x,4,z) -> [(2,3,z-1), (x-1,4,z), (x+1,4,z), (x,3,z)]

                  -- everyone else
                  _       -> [s +: c | c <- adjacents]

updateGrid, updateGridA :: Grid -> Grid
updateGrid m = M.mapWithKey (\k v -> rule m k) m
updateGridA m = M.mapWithKey (\k v -> ruleA m k) m

parseInput :: String -> [Coord]
parseInput s = coords
  where
    coords    = [(i,j,0) | (i,ls) <- idxLines
                         , (j,c) <- zip [0..] ls
                         , c == '#']
    idxLines  = zip [0..] (lines s)

example = [(0,4,0), (1,0,0), (1,3,0), (2,0,0), (2,3,0), (2,4,0), (3,2,0), (4,0,0)]

createGrid :: Int -> [Coord] -> Grid
createGrid lvls cs = M.fromList $ [(c, f c) | c <- coords] ++ recurse
  where
    coords  = [(i,j,0) | j <- [0..size-1], i <- [0..size-1]]
    f c'    = if c' `elem` cs then Alive else Dead
    recurse = [((i,j,z), Dead) | i <- [0..size-1]
                               , j <- [0..size-1]
                               , z <- [-lvls..lvls]
                               , z /= 0]

life, lifeA :: Grid -> [Grid]
life  = iterate updateGrid
lifeA = iterate updateGridA

findFirstRepeat :: Ord a => [a] -> a
findFirstRepeat xs = go xs S.empty
  where
    go (y:ys) s = if   S.member y s
                  then y
                  else go ys (S.insert y s)

addClear s = "\ESC[2J" ++ s
size = 5

calculateEnergy :: Grid -> Int
calculateEnergy m = sum $ M.keys energy
  where
    energy = M.mapKeys (\(x,y,z) -> 2^(y + x*size)) alive
    alive  = M.filter (==Alive) m

countBugs :: Grid -> Int
countBugs m = length $ M.toList onlyBugs
  where
    onlyBugs     = (M.filter (==Alive) . M.filterWithKey p) m
    p  (x,y,z) v = (x,y)/= (2,2)

day24 :: IO ()
day24 = do
  dat <- readFile "day24.txt"
  let
    st    = parseInput dat
    gameA = lifeA $ createGrid 0 st
    game  = life $ createGrid 100 st
    bugs  = countBugs (game !! 200)
    fstRepeat = findFirstRepeat $ displayGrid 0 <$> game
  putStrLn $ displayGrid 0 $ head game
  putStrLn fstRepeat
  print $ calculateEnergy $ findFirstRepeat gameA
  print bugs
  putStrLn $ displayGrid 0 (game !! 200)
