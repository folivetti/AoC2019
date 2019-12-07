module Day06 where

import qualified Data.Map as M
import Data.List.Split (splitOn)

type OrbitMap = M.Map String String

example = ["COM)B","B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU","I)SAN"]

toMap :: [String] -> OrbitMap
toMap css = M.fromList $ fmap (toTuple . splitOn ")") css
  where
    toTuple (x:y:_) = (y,x)

lengthToCom :: String -> OrbitMap -> Int
lengthToCom "COM" orbit = 0
lengthToCom k orbit     = 1 + lengthToCom (orbit M.! k) orbit

comToList :: String -> OrbitMap -> [String]
comToList "COM" orbit = ["COM"]
comToList k orbit     = k : comToList (orbit M.! k) orbit

distance :: String -> String -> OrbitMap -> Int
distance k1 k2 orbit = n1 + n2 -(2*n) - 2 
  where
    l1 = reverse $ comToList k1 orbit
    l2 = reverse $ comToList k2 orbit
    common = takeWhile (uncurry (==)) $ zip l1 l2
    n1 = length l1
    n2 = length l2
    n  = length common

day06 :: IO ()
day06 = do
  dat <- lines <$> readFile "day06.txt"
  let orb = toMap dat 
  print $ sum [lengthToCom k orb | k <- M.keys orb]
  print $ distance "YOU" "SAN" orb
