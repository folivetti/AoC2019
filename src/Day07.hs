module Day07 where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.List (permutations, sortOn, intercalate)

type Tape = M.Map Int Int
data Machine = M { tape :: Tape
                 , cursor :: Int
                 , input :: [Int]
                 , output :: Maybe Int
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
runReader m@(M t c (i:is) o _) = m{ tape = t', cursor = c+2, input = is }
  where
    arg1 = t M.! (c+1)
    t'   = M.insert arg1 i t
runReader m@(M t c [] o _) = error (show t ++ show o ++ "No input") -- m{ tape = t', cursor = c+2, input = is }

runWriter :: Machine -> Machine
runWriter m@(M t c _ o _) = m{ cursor = c+2, output = Just x }
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

runMachineWith :: [Int] -> Machine -> Machine
runMachineWith xs m = runMachine m{ input=xs }

runMachineOutput :: Machine -> [Int]
runMachineOutput m | finished = case o of
                                  Nothing -> []
                                  Just x  -> [x]
                   | otherwise = case o of
                                   Nothing -> runMachineOutput m'{output = Nothing}
                                   Just x  -> x : runMachineOutput m'{output = Nothing}
  where
    m' = step m
    o  = output m' 
    finished = cursor m' == size m'

allSettings = permutations [0,1,2,3,4]
allSettings2 = permutations [5,6,7,8,9]

runPhaseTest :: Int -> [Int] -> Machine -> Int
runPhaseTest o0 [p1,p2,p3,p4,p5] m = head o5 
  where
    o1 = runMachineOutput m{input = [p1,o0]}
    o2 = runMachineOutput m{input = [p2,head o1]}
    o3 = runMachineOutput m{input = [p3,head o2]}
    o4 = runMachineOutput m{input = [p4,head o3]}
    o5 = runMachineOutput m{input = [p5,head o4]}
runPhaseTest _ _ _ = error "incorrect permutation"

fromMaybe (Just x) = x
fromMaybe Nothing  = error "No input"

runFeedBackTest :: [Int] -> Machine -> Int
runFeedBackTest [p1,p2,p3,p4,p5] m = last o5
   where
    o1 = runMachineOutput m{input = p1 : 0 : o5}
    o2 = runMachineOutput m{input = p2 : o1}
    o3 = runMachineOutput m{input = p3 : o2}
    o4 = runMachineOutput m{input = p4 : o3}
    o5 = runMachineOutput m{input = p5 : o4}

runAllTests :: [[Int]] -> Machine -> (Int, [Int])
runAllTests perms m = head $ sortOn (negate . fst) [(runPhaseTest 0 p m, p) | p <- perms ]

runAllFeedbackTests :: [[Int]] -> Machine -> (Int, [Int])
runAllFeedbackTests perms m = head $ sortOn (negate . fst) [(runFeedBackTest p m, p) | p <- perms ]

excodes = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
excodes2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

day07 :: IO ()
day07 = do
  dat <- splitOn "," <$> readFile "day07.txt" 
  let
    codes = fmap read dat :: [Int]
    tape  = M.fromList (zip [0..] codes)
    m     = M tape 0 [1] Nothing (length codes)
  --print $ runAllTests allSettings m
  print $ runAllFeedbackTests allSettings2 m
