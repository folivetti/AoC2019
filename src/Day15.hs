module Day15 where

import IntCode
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Map.Strict as M

type Track = M.Map (Int, Int) Int
type Coord = (Int, Int)

makeMove 1 (x,y) = (x, y+1)
makeMove 2 (x,y) = (x, y-1)
makeMove 3 (x,y) = (x-1,y)
makeMove 4 (x,y) = (x+1,y)

hasCycle [] = False
hasCycle ms = hasCycle' ms [(0,0)]
  where
    hasCycle' [] coords = False
    hasCycle' (m:ms) coords = c `elem` coords || hasCycle' ms (c:coords)
      where c = makeMove m (head coords)

isGoal :: (Int, SEQ) -> Bool
isGoal (2, _) = True
isGoal _      = False

isNotFinal :: (Int, SEQ) -> Bool
isNotFinal (0,_) = False
isNotFinal _     = True

move :: Machine -> Int -> (Int, [Int]) -> (Int, [Int])
move m x (st, t) = (status, x:t)
  where
    (m', os) = runUntilRead m{ input=reverse (x:t) } []
    status   = if 0 `elem` os then 0 else last os

moves m = pure (move m) <*> [1,2,3,4]

-- | astar [(Blocks, Moves)]
type SEQ = [Int]

breadth :: Machine -> [(Int, SEQ)] -> SEQ
breadth m [] = error "Couldn't find feasible solution"
breadth m sts | (not.null) goal     = snd $ head goal
              | otherwise           = breadth m (filter (not.hasCycle.snd) sts')
  where
    goal        = filter isGoal sts
    stsNotFinal = filter isNotFinal sts
    sts'        = moves m <*> stsNotFinal

backtrack :: ([(Coord, Machine)], Track) -> Int
backtrack (ms, t)

  | null res     = 0
  |otherwise     = 1 + backtrack (foldl joinRes ([], t) res)
    where
      joinRes (xs, t) (cm, t') = (cm:xs, M.union t t')
      res                      = concatMap (runAllInputs t) ms
      runAllInputs t cm        = mapMaybe (expandNode t cm) [1..4]

expandNode t (c,m) x = case mo of
                         Nothing       -> Nothing 
                         Just (m', os) -> insert m' (last os)
  where
    c' = makeMove x c
    insert m' o = if o==0 || o==2 then Nothing
                  else Just ((c',m'), M.insert c' o t)
    mo = if c' `M.member` t 
         then Nothing 
         else Just (runUntilRead m{ input=[x] } [])


day15 :: IO ()
day15 = do
  dat <- splitOn "," <$> readFile "day15.txt"
  let
    codes   = fmap read dat :: [Int]
    m       = createMachineFromList codes
    sol     = breadth m [(1,[])]
    (m', o) = runUntilRead m{ input=reverse sol } []
  print sol
  print $ length sol
  print $ backtrack ([((0,0), m')], M.singleton (0,0) 2)
