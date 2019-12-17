module Day14 where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.List (sortOn, groupBy)

type Qty         = Int
type Leftovers   = M.Map String Int
type Ingredients = [(String, Qty)]
type Materials   = M.Map String (Qty, Ingredients) 

parseLine :: String -> (String, (Qty, [(String, Qty)]))
parseLine css = (product, (qtd, fmap parsePairs materials))
  where
    (cs1:cs2:_) =  splitOn " => " css
    materials   =  splitOn ", "  cs1
    (product, qtd) = parsePairs cs2

parsePairs :: String -> (String, Int)
parsePairs css = (css', read x)
  where
    (x:css':_) = splitOn " " css

getDefDict = M.findWithDefault 0

sequenceFun m leftovers [] x          = (x, leftovers)
sequenceFun m leftovers ((k,v):ingredients) x = sequenceFun m lft ingredients (x+z)
  where
    (z, lft) = howManyOres m leftovers k v

howManyOres :: Materials -> Leftovers -> String -> Qty -> (Qty, Leftovers)
howManyOres m leftovers "ORE" x = (x, leftovers)
howManyOres m leftovers k x     
  | getDefDict k leftovers >= x = (0, M.insert k (-x') leftovers) 
  | otherwise                   = sequenceFun m lft' ingredients' 0
    where
      x'                 = x - getDefDict k leftovers
      lft                = M.insert k 0 leftovers
      (qty,ingredients)  = m M.! k
      (q, r)             = x' `quotRem` qty
      multiplier         = q + (if r/=0 then 1 else 0)
      lft'               = M.insert k (max 0 (multiplier*qty - x')) lft
      applyToSnd z (a,b) = (a, b*z)
      ingredients'       = fmap (applyToSnd multiplier) ingredients
     
howManyOres' m l k x = fst $ howManyOres m l k x
 
howManyFuel materials low stp = (fst.last) res
  where
    res   = zip bound $ takeWhile (<1000000000000) ores
    ores  = fmap (howManyOres' materials M.empty "FUEL") bound
    bound = [low, (low+stp)..]

day14 :: IO ()
day14 = do
  dat <- lines <$> readFile "day14.txt"
  let
    materialList = parseLine <$> dat
    materials    = M.fromList materialList
    leftovers    = M.empty
    orePerFuel   = fst $ howManyOres materials leftovers "FUEL" 1
    thousands    = howManyFuel materials 1000 1000
    hundreds     = howManyFuel materials thousands 100
    units        = howManyFuel materials hundreds 1
  print orePerFuel 
  print units 
