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

processAllOutputs :: [Int] -> OutQueue -> OutQueue
processAllOutputs []          q = q
processAllOutputs (i:x:y:oss) q = processAllOutputs oss q'
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

getNextNat :: OutQueue -> Maybe [Int] -> Maybe [Int]
getNextNat q nat = case q M.!? 255 of
                     Just (x :<| qs) -> Just x
                     _               -> nat

toMaybe :: Bool -> Maybe a -> Maybe a
toMaybe b v = if b then v else Nothing

updateIf :: Bool -> Machines -> OutQueue -> Maybe [Int] -> Machines
updateIf b ms q nat = if b
                      then M.insert 0 m{ input=fromMaybe [] nat } ms
                      else M.mapWithKey (\k m -> m{ input=getOutput k q }) ms
  where
    m = ms M.! 0

stepNetwork :: (Machines, OutQueue, Maybe [Int]) -> (Machines, OutQueue, Maybe [Int])
stepNetwork (ms, q, nat) = (ms', q', nat')
  where
    run    = M.map (`runUntilRead` []) ms          -- run machines with current inputs: M.Map Int (Machine, Out)
    os     = concatMap snd $ M.elems run           -- concat all outputs
    q'     = processAllOutputs os (popOutputs q)   -- remove used inputs and enqueue new ones
    isIdle = all (==Empty) (init $ M.elems q')     -- are the machines idle?
    nat'   = toMaybe isIdle (getNextNat q' nat)
    ms'    = updateIf isIdle (M.map fst run) q' nat'

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

    running  = iterate stepNetwork (net, queue, Nothing)
    answer   = (filter isJust . map (\(_,_,n) -> n)) running

  print $ head answer
  print $ (getRepeat . map ((!!1).fromJust)) answer
