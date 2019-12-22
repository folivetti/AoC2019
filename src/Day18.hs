module Day18 where

import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char

data State = S { cur   :: [(Int,Int)]
               , steps :: Int
               , keys  :: S.Set Char
               } deriving (Show, Eq, Ord)

parseFile :: String -> M.Map (Int, Int) Char
parseFile dat = M.fromList datWithCoords
  where
    nLines  = length $ lines dat
    nCols   = length $ head (lines  dat)
    flatDat = filter (/='\n') dat
    datWithCoords = (\(i,s) -> (coord i, s)) <$> zip [0..] flatDat
    coord x       = (x `rem` nCols, x `div` nCols)

isTerminal :: M.Map (Int,Int) Char -> State -> Bool
isTerminal m (S cur s k) = ('#' `elem` cs)  || (any (\c -> isUpper c && not (toLower c `S.member` k)) cs)
  where
    cs  = fmap (m M.!) cur

isGoal :: State -> Bool
isGoal s = S.size (keys s) == 26

tupleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleAdd (x,y) (z,w) = (x+z, y+w)

go :: [(Int,Int)] -> M.Map (Int, Int) Char -> State -> State
go coords m st@(S cur s k) = S cur' (s+1) k'
  where
    cur' = zipWith tupleAdd cur coords
    k'  = S.union k (S.fromList (filter isLower $ fmap (m M.!) cur'))


directions = [(-1,0), (1,0), (0,-1), (0,1)]

pathA = pure (:[(0,0),(0,0),(0,0)]) <*> directions
genDiags n x = [ [if i==j then x else (0,0) | j <- [1..n]] | i <- [1..n] ]


step :: M.Map (Int, Int) Char -> S.Set ((Int, Int), S.Set Char) -> [State] -> (S.Set ((Int,Int),S.Set Char), [State])
step m tabu []  = error "Could not find a solution! Xmas is ruined!"
step m tabu sts = (tabu', sts')
  where
    coords    = concatMap (genDiags n) directions
    n         = (length . cur . head) sts
    nextSts   = pure go <*> coords <*> [m] <*> sts
    notEq     = nub $ filter (not . all (`S.member` tabu) . stToTab)  nextSts
    sts'      = filter (not.isTerminal m) notEq
    tabu'     = S.unions (tabu:fmap stToTab sts')
    stToTab s = S.fromList [(s', keys s) | s' <- cur s]

breadth :: M.Map (Int, Int) Char -> S.Set ((Int, Int), S.Set Char) -> [State] -> State
breadth m tabu [] = error "Could not find a solution! Xmas is ruined!"
breadth m tabu sts = head $ filter isGoal $ snd $ head sol
  where
    hasNoGoal s  = not (any isGoal s) 
    sol          = dropWhile (hasNoGoal.snd) $ iterate (uncurry (step m)) (tabu, sts)

day18 :: IO ()
day18 = do
  dat <- readFile "day18.txt"
  let
    m          = parseFile dat
    startPoint = fst.head $ filter ((=='@').snd)  $ M.toList m
    s0         = S [startPoint] 0 S.empty
    debug      = iterate (uncurry (step m)) (S.fromList [(startPoint, S.empty)], [s0])
    pointsB    = fmap (tupleAdd startPoint) [(-1,-1), (-1,1), (1,-1), (1,1)]
    wallsB     = zip (fmap (tupleAdd startPoint) [(-1,0), (0,1), (0,-1), (1,0), (0,0)]) (repeat '#')
    sB         = S pointsB 0 S.empty
    m'         = M.union (M.fromList wallsB) m
    tabu       = S.fromList [(p,S.empty) | p <- pointsB]
  putStrLn dat
  print startPoint
--  print $ breadth m (S.fromList [(startPoint, S.empty)]) [s0] 
  print $ breadth m' tabu [sB] 
