module Day13 where

import IntCode
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Map.Strict as M

gameScreen :: [Int] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
gameScreen [] m = m
gameScreen (x:y:c:os) m  = gameScreen os (M.insert (x,y) c m)

play :: Machine -> M.Map (Int, Int) Int -> (M.Map (Int,Int) Int, Int)
play m screen | isHalted m' = (screen', score)
              | otherwise   = play m' screen'
  where
    (m', os) = runUntilRead m{ input=[inp] } []
    screen'  = gameScreen os screen
    score    = screen' M.! (-1,0)
    xPaddle  = (fst.fst.head) $ filter ((==3).snd) $ M.assocs screen
    xBall    = (fst.fst.head) $ filter ((==4).snd) $ M.assocs screen
    inp      = case compare xPaddle xBall of
                 LT -> 1
                 GT ->  -1
                 _  ->  0

countBlocksFromScreen :: M.Map (Int,Int) Int -> Int
countBlocksFromScreen screen = M.size $ M.filter (==2) screen

playAstar :: Machine -> M.Map (Int,Int) Int -> [Int] -> Int
playAstar m screen []     = countBlocksFromScreen screen 
playAstar m screen (i:is) |  isHalted m' = countBlocksFromScreen screen
                          | otherwise    = playAstar m' screen' is
  where
    (m', os) = runUntilRead m{ input=[i] } []
    screen'  = gameScreen os screen
   

isGoal :: ((Int, Int), [Int]) -> Bool
isGoal ((0,_), _) = True
isGoal _          = False

move :: Machine -> Int -> ((Int, Int), [Int]) -> ((Int,Int), [Int])
move m x ((bl, sc), t) = ((blocks, score), x:t)
  where
    (m', os) = runUntilRead m{ input=[] } []
    screen   = gameScreen os M.empty
    blocks   = playAstar m screen (x:t)
    score    = screen M.! (-1,0)

moves m = pure (move m) <*> [-1,0,1]

h :: ((Int,Int), [Int]) -> Int
h ((s,_), t) = s + length t

argmin = snd . minimum

-- | astar [(Blocks, Moves)]
astar :: Machine -> [((Int,Int), [Int])] -> ((Int,Int), [Int])
astar m [] = error "Couldn't find feasible solution"
astar m sts | (not.null) goal = head goal
            | otherwise       = astar m sts'
  where
    goal = filter isGoal sts
    sts' = filter (/=s') $ sts ++ (moves m <*> [s'])
    s'   = argmin $ zip (map h sts) sts


day13 :: IO ()
day13 = do
  dat <- splitOn "," <$> readFile "day13.txt"
  let
    codes             = fmap read dat :: [Int]
    m                 = createMachineFromList codes
    m'                = createMachineFromList (2 : tail codes)
    os                = runMachineGenerator m
    screen            = gameScreen os M.empty
    (screen', score') = play m' screen
    blocks            = countBlocksFromScreen screen
  print blocks
  print score' 
  print $ astar m' [((blocks,0), [])]
