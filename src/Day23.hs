module Day23 where

import IntCode
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Sequence ((|>), Seq(..) )
import qualified Data.Sequence   as Q
import Data.Foldable
import Data.Maybe

type OutQueue = M.Map Int (Q.Seq [Int])
type Machines = M.Map Int Machine

processOutput :: OutQueue -> [Int] -> OutQueue
processOutput q [i,x,y] = q'
  where
    qi = case q M.!? i of
           Nothing -> Q.singleton [x,y]
           Just z  -> z |> [x,y]
    q' = M.insert i qi q
processOutput q os = error (show os)

processAllOutputs :: OutQueue -> [Int] -> OutQueue
processAllOutputs q []       = q
processAllOutputs q (i:x:y:oss) = processAllOutputs q' oss
  where
    q' = processOutput q [i,x,y]

getOutput :: Int -> OutQueue -> [Int]
getOutput k q = case q M.!? k of
                  Nothing         -> [-1]
                  Just Empty      -> [-1]
                  Just (x :<| q') -> x

popOutputs :: OutQueue -> OutQueue
popOutputs = M.map pop
  where
    pop qi = case qi of
               Empty    -> Empty
               x :<| q' -> q'


stepNetwork :: (Machines, OutQueue) -> (Machines, OutQueue)
stepNetwork (ms, q) = (ms', q'')
  where
    run  = M.map (`runUntilRead` []) ms          -- run machines with current inputs: M.Map Int (Machine, Out)
    os   = concatMap snd $ M.elems run           -- concat all outputs
    q'   = popOutputs q                          -- remove used inputs
    q''  = processAllOutputs q' os               -- insert outputs into queue
    ms'  = M.mapWithKey (\k (m,o) -> m{ input=getOutput k q'' }) run

stepNetwork2 :: (Machines, OutQueue, ([Int], Bool)) -> (Machines, OutQueue, ([Int], Bool))
stepNetwork2 (ms, q, (nat,_)) = (ms', q'', (nat',b))
  where
    run  = M.map (`runUntilRead` []) ms          -- run machines with current inputs: M.Map Int (Machine, Out)
    os   = concatMap snd $ M.elems run           -- concat all outputs
    q'   = popOutputs q                          -- remove used inputs
    q''  = processAllOutputs q' os               -- insert outputs into queue
    nat' = case q'' M.!? 255 of
             Nothing -> nat
             Just Empty -> nat
             Just (x :<| qs) -> x
    tmp  = M.map fst run
    b    = all (==Empty) (init $ M.elems q'')
    ms'  = if   b 
           then M.insert 0 ((tmp M.! 0){ input=nat' }) tmp
           else M.mapWithKey (\k (m,o) -> m{ input=getOutput k q'' }) run

getRepeat []                   = 0
getRepeat (x:y:xs) | x==y      = x
                   | otherwise = getRepeat (y:xs)

day23 :: IO ()
day23 = do
  dat <- splitOn "," <$> readFile "day23.txt"
  let
    codes = fmap read dat
    m     = createMachineFromList codes
    net   = M.fromList [ (i, m{ input=[i,-1] }) | i <- [0..49] ]
    queue = M.fromList [ (i, Q.empty)           | i <- [0..49] ]

    running  = iterate stepNetwork (net, queue)
    answerA  = dropWhile (\(ms,q) -> isNothing (q M.!? 255)) running

    running2 = iterate stepNetwork2 (net, queue, ([], False))
    answerB  = map (\(_,_,n) -> n) running2
  print $ snd $ head answerA
  print $ getRepeat  $ map ((!!1).fst) $ filter snd $ filter (not.null.fst) answerB
