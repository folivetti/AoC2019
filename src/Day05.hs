module Day05 where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)

type Tape = M.Map Int Int
data Machine = M { tape :: Tape
                 , cursor :: Int
                 , input :: Int
                 , output :: Int
                 , size :: Int 
                 }

data Op = Add | Mul | Read | Write | JmpNZ | JmpZ | Cmp | Equal | STOP

step :: Machine -> Machine
step m = m'
  where
    m' = case opCode m of
            Add   -> runMath (+) m
            Mul   -> runMath (*) m -- :-)
            Read  -> runReader m
            Write -> runWriter m
            JmpNZ -> runJumper (/=0) m
            JmpZ  -> runJumper (==0) m
            Cmp   -> runCmp (<) m
            Equal -> runCmp (==) m
            STOP  -> m { cursor = size m }

opCode :: Machine -> Op
opCode (M t c _ _ _) = toOpCode $ extractOp (t M.! c)
  where
    extractOp x = x `rem` 100
    toOpCode 1 = Add
    toOpCode 2 = Mul
    toOpCode 3 = Read
    toOpCode 4 = Write
    toOpCode 5 = JmpNZ
    toOpCode 6 = JmpZ
    toOpCode 7 = Cmp
    toOpCode 8 = Equal
    toOpCode 99 = STOP

runMath :: (Int -> Int -> Int) -> Machine -> Machine
runMath op m@(M t c _ _ _) = m{ tape = t', cursor = c+4 }
  where
    modes = (t M.! c) `div` 100
    mode1 = modes `rem` 10
    mode2 = (modes `div` 10) `rem` 10
    arg1 = t M.! (c+1)
    arg2 = t M.! (c+2)
    arg3 = t M.! (c+3)
    x    = if mode1 == 0 then t M.! arg1 else arg1
    y    = if mode2 == 0 then t M.! arg2 else arg2
    t'   = M.insert arg3 (x `op` y) t

runReader :: Machine -> Machine
runReader m@(M t c i _ _) = m{ tape = t', cursor = c+2 }
  where
    arg1 = t M.! (c+1)
    t'   = M.insert arg1 i t

runWriter :: Machine -> Machine
runWriter m@(M t c _ o _) = m{ cursor = c+2, output = x }
  where
    modes = (t M.! c) `div` 100
    mode1 = modes `rem` 10
    arg1  = t M.! (c+1)
    x     = if mode1 == 0 then t M.! arg1 else arg1

runJumper :: (Int -> Bool) -> Machine -> Machine
runJumper cmp m@(M t c _ _ _) = m{ cursor = if cmp x then y else c + 3 }
  where
    modes = (t M.! c) `div` 100
    mode1 = modes `rem` 10
    mode2 = (modes `div` 10) `rem` 10
    arg1 = t M.! (c+1)
    arg2 = t M.! (c+2)
    x    = if mode1 == 0 then t M.! arg1 else arg1
    y    = if mode2 == 0 then t M.! arg2 else arg2
    
runCmp :: (Int -> Int -> Bool) -> Machine -> Machine
runCmp op m@(M t c _ _ _) = m{ tape = t', cursor = c+4 }
  where
    modes = (t M.! c) `div` 100
    mode1 = modes `rem` 10
    mode2 = (modes `div` 10) `rem` 10
    arg1 = t M.! (c+1)
    arg2 = t M.! (c+2)
    arg3 = t M.! (c+3)
    x    = if mode1 == 0 then t M.! arg1 else arg1
    y    = if mode2 == 0 then t M.! arg2 else arg2
    t'   = if x `op` y then M.insert arg3 1 t else M.insert arg3 0 t

runMachine :: Machine -> Machine
runMachine m | cursor m' == size m' = m'
             | otherwise            = runMachine m'
  where m' = step m

runMachineWith :: Int -> Machine -> Machine
runMachineWith x m = runMachine m{ input=x }

day05 :: IO ()
day05 = do
  dat <- splitOn "," <$> readFile "day05.txt" 
  let
    codes = fmap read dat
    tape  = M.fromList (zip [0..] codes)
    m     = M tape 0 1 0 (length codes)    
  print $ output (runMachineWith 1 m) 
  print $ output (runMachineWith 5 m) 
