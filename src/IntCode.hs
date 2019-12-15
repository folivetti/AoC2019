module IntCode where

import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Data.List (permutations, sortOn, intercalate)
import Data.Maybe

-- | A tape is a TreeMap - it efficiently manages
-- the theoretical infinite size
type Tape = M.Map Int Int
data Machine = M { tape   :: Tape          -- current state
                 , cursor :: Int           -- cursor position
                 , input  :: [Int]         -- input buffer
                 , output :: Maybe Int     -- output buffer
                 , base   :: Int           -- base address
                 , size   :: Int           -- size of tape
                 } deriving Show

-- | Creates a machine from a list of IntCodes
createMachineFromList :: [Int] -> Machine
createMachineFromList xs = M t 0 [] Nothing 0 (length t)
  where
    t = M.fromList $ zip [0..] xs

-- | Allowed operations of the machine. NoOP is here just to make sure
-- we start counting opcodes from 1
-- Deriving Enum make it so `toEnum 1 = Add`, `toEnum 2 = Mul`, etc.
data Op = NoOP | Add | Mul | Read | Write | JmpNZ | JmpZ | Cmp | Equal | Base | STOP
            deriving (Enum, Show)

-- | Perform one step of execution of a machine
-- it returns the new state and maybe an output
step :: Machine -> (Machine, Maybe Int)
step m = (m', output m')
  where
    m' = case opCode m of
            Add   -> runMath (+) m
            Mul   -> runMath (*) m 
            Read  -> runReader m
            Write -> runWriter m
            JmpNZ -> runJumper (/=0) m
            JmpZ  -> runJumper (==0) m
            Cmp   -> runCmp (<) m
            Equal -> runCmp (==) m
            Base  -> changeBase m
            STOP  -> m { cursor = size m }

-- Get last two digits and convert to an Op code
opCode :: Machine -> Op
opCode (M t c _ _ _ _) = toOpCode $ (t M.! c) `rem` 100
  where
    toOpCode 99 = STOP
    toOpCode x  = toEnum x

-- | get the read/write argument given an argument mode
getReadArg, getWriteArg :: Tape 
                        -> Int  -- Cursor
                        -> Int  -- nArg
                        -> Int  -- Base
                        -> Int  -- Mode
                        -> Int  -- argument
getReadArg t c nArg _ 0 = M.findWithDefault 0 k t
  where k = t M.! (c+nArg)
getReadArg t c nArg _ 1 = k
  where k = t M.! (c+nArg)
getReadArg t c nArg b 2 = M.findWithDefault 0 (k+b) t
  where k = t M.! (c+nArg)

getWriteArg t c nArg b 2 = k + b
  where k = t M.! (c+nArg)
getWriteArg t c nArg _ _ = k
  where k = t M.! (c+nArg)

-- extract the n-th mode from an opcode
getMode :: Int -> Int -> Int
getMode x n = (x `div` 10^(n-1)) `rem` 10

-- | Run mathematical operations
runMath :: (Int -> Int -> Int) -> Machine -> Machine
runMath op m@(M t c _ _ b _) = m{ tape = t', cursor = c+4 }
  where    
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    mode2 = getMode modes 2
    mode3 = getMode modes 3
    x    = getReadArg t c 1 b mode1
    y    = getReadArg t c 2 b mode2
    z    = getWriteArg t c 3 b mode3
    t'   = M.insert z (x `op` y) t

-- | Run reading operation
runReader :: Machine -> Machine
runReader m@(M t c (i:is) o b _) = m{ tape = t', cursor = c+2, input = is }
  where
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    t'   = M.insert (getWriteArg t c 1 b mode1) i t
runReader m@(M t c [] o _ _) = error (show t ++ show o ++ " No input") 

-- | Run writing operation
runWriter :: Machine -> Machine
runWriter m@(M t c _ o b _) = m{ cursor = c+2, output = Just x }
  where
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    x     = getReadArg t c 1 b mode1

-- | Run jump operation
runJumper :: (Int -> Bool) -> Machine -> Machine
runJumper cmp m@(M t c _ _ b _) = m{ cursor = if cmp x then y else c + 3 }
  where
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    mode2 = getMode modes 2
    x    = getReadArg t c 1 b mode1
    y    = getReadArg t c 2 b mode2
    
-- | Run comparison operations
runCmp :: (Int -> Int -> Bool) -> Machine -> Machine
runCmp op m@(M t c _ _ b _) = m{ tape = t', cursor = c+4 }
  where
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    mode2 = getMode modes 2
    mode3 = getMode modes 3
    x    = getReadArg t c 1 b mode1
    y    = getReadArg t c 2 b mode2
    z    = getWriteArg t c 3 b mode3
    t'   = if x `op` y then M.insert z 1 t else M.insert z 0 t

-- | Change the current base
changeBase :: Machine -> Machine
changeBase m@(M t c _ _ b _) = m{ base=b+base, cursor = c+2 }
  where
    modes = (t M.! c) `div` 100
    mode1 = getMode modes 1
    base     = getReadArg t c 1 b mode1

-- | Run the machine until STOP code and maybe return an output
runMachine :: Machine -> (Machine, Maybe Int)
runMachine m | cursor m' == size m' = (m', mo)
             | otherwise            = runMachine m'
  where (m', mo) = step m

-- | run the IntCode Machine while providing
-- the output as a data stream
runMachineGenerator :: Machine -> [Int]
runMachineGenerator m | finished = case mo of
                                     Nothing -> []
                                     Just x  -> [x]
                      | otherwise = case mo of
                                      Nothing -> runMachineGenerator m'
                                      Just x  -> x : runMachineGenerator m'{output = Nothing}
  where
    (m', mo) = step m
    finished = cursor m' == size m'
