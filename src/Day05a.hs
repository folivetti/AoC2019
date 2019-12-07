module Main where

import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M

-- | Day 05
type Tape = M.Map Int Int
data Mode = Pos | Val
data Op = Add | Mul | Read | Write | JmpIfT | JmpIfF | STOP
type Machine = (Tape, Int, Int)

nArgs 1 = 3
nArgs 2 = 3
nArgs 3 = 1
nArgs 4 = 1
nArgs _ = 0

runMachine :: Machine -> Machine
runMachine m = case execCode m of
  Just m'  -> runMachine m'
  Nothing  -> m                  

execCode :: Machine -> Maybe Machine
execCode (tape, output, pos) = m'
  where
    opcode = tape M.! pos
    m'     = case op of
              Add   -> Just (runAdd (getArgs (extractArgs 3) modes) tape, output, pos + 4)
              Mul   -> Just (runMul (getArgs (extractArgs 3) modes) tape, output, pos + 4)
              Write -> Just (runWrite [tape M.! (pos+1)] tape, output, pos + 2)
              Read  -> Just $ (\(x,y) -> (x,y,pos+2)) (runRead [tape M.! (pos+1)] tape)
              STOP  -> Nothing

    extractArgs d = fmap (tape M.!) [pos + 1 .. pos +  d]
    (op, modes) = translateOp opcode
    getArgs [] _ = []
    getArgs [a] _  = [a]
    getArgs (a:as) [] = tape M.!  a : getArgs as []
    getArgs (a:as) (Pos:ms) = tape M.! a : getArgs as ms
    getArgs (a:as) (Val:ms) = a : getArgs as ms

translateOp :: Int -> (Op, [Mode])
translateOp x | x < 100   = (op, [Pos, Pos, Pos])
              | otherwise = (op, modes)
  where
    op = toOp $ x `rem` 100
    modes = fmap (toMode.snd) . tail . takeWhile (/=(0,0)) . iterate ((`quotRem` 10).fst) $ (x `quot` 100, 0)

toMode 0 = Pos
toMode 1 = Val
toMode _ = Pos

toOp 1 = Add
toOp 2 = Mul
toOp 3 = Write
toOp 4 = Read
toOp 99 = STOP
toOp _ = STOP

runAdd :: [Int] -> Tape -> Tape
runAdd [x,y,z] tape = M.insert z (x+y) tape
runAdd xs tape = error ("Not enough args for add" ++ show xs)

runMul :: [Int] -> Tape -> Tape
runMul [x,y,z] = M.insert z (x*y)
runMul _ = error "Not enough args for mul"

runWrite :: [Int] -> Tape -> Tape
runWrite [x] = M.insert x 1
runWrite _ = error "Not enough args for write"

runRead :: [Int] -> Tape -> (Tape, Int)
runRead [x] tape = (tape, tape M.! x)
runRead _ _ = error "Not enough args for read"

main :: IO ()
main = do
  day05 <- fmap read . splitOn "," <$> readFile "day05.txt" :: IO [Int]
  let 
    tape = M.fromList $ zip [0..] day05 
    (tape', output, pos) = runMachine (tape, 0, 0)
  print output
