module Day10 where

import Data.List (sortOn)
import qualified Data.Map.Strict as M

type Coord =  (Int,Int)
type DistMap = M.Map (Coord, Coord) Double

data Space = Empty | Asteroid
               deriving Eq

instance Show Space where
  show Empty    = "."
  show Asteroid = "#"

readSpace :: Char -> Space
readSpace '.' = Empty
readSpace '#' = Asteroid

spaceToCoords :: [[Space]] -> [(Int,Int)]
spaceToCoords css = [(x,y) | (y,cs) <- withIdx
                           , (x,c)  <- cs
                           , c == Asteroid]
  where 
    withIdx = zip [0..] (fmap (zip [0..]) css)

angle :: Coord -> Coord -> Double
angle (x,y) (z,w) = if ang < 0 then ang + 360 else ang
  where
    delta1 = fromIntegral (z-x)
    delta2 = fromIntegral (y-w)
    arc    = atan2 delta1 delta2
    ang    = arc * 180 / pi

euclidean :: Coord -> Coord -> Double
euclidean (x,y) (z,w) = sqrt . fromIntegral $ (x-z)^2 + (y-w)^2

-- | not numeric stable
canMonitor :: Coord -> Coord -> [Coord] -> DistMap -> Bool
canMonitor a b coords dists = all isNotBetween candidates
  where
    dist           = dists M.! (a,b)
    isNotBetween c = abs (dists M.! (a,c) + dists M.! (c,b) - dist) > 1e-10
    candidates     = [c | c <- coords, c /= a, c /= b]

calcAllDistances :: [Coord] -> DistMap
calcAllDistances coords = M.fromList [((c1,c2), euclidean c1 c2) | c1 <- coords
                                                                 , c2 <- coords
                                                                 , c1 /= c2]

numberOfDetections :: [Coord] -> DistMap -> Coord -> Int
numberOfDetections coords dists c = length [c' | c' <- coords
                                           , c /= c'
                                           , canMonitor c c' coords dists]

getAllDetections :: DistMap -> [Coord] -> [(Coord,Int)]
getAllDetections dists coords = zip coords detections
  where
    detections = numberOfDetections coords dists <$> coords

getMaxDetections :: [(Coord,Int)] -> (Coord, Int)
getMaxDetections (c:cs) = getMax cs c
  where
    getMax [] maxC       = maxC
    getMax (c':cs') maxC = if   snd c' > snd maxC
                           then getMax cs' c'
                           else getMax cs' maxC


coordsWithAngleDist :: [Coord] -> DistMap -> Coord -> [((Double, Double), Coord)]
coordsWithAngleDist coords dists c = sortOn fst [((angle c ci, dists M.! (c,ci)), ci) | ci <- coords ]

data ZipList a = Z [a] [a]

getDestroyingList :: ZipList ((Double,Double), Coord) -> [Coord]
getDestroyingList (Z [] []) = []
getDestroyingList (Z ls (r:rs)) = snd r : getDestroyingList z'
  where
    getAng = fst.fst
    ang    = getAng r
    ls'    = ls ++ takeWhile ((==ang).getAng) rs
    rs'    = dropWhile ((==ang).getAng) rs
    z'     = if null rs' then Z [] ls' else Z ls' rs'

example01 = ".#..#\n.....\n#####\n....#\n...##"

day10 :: IO ()
day10 = do
  dat <- readFile "day10.txt"
  let
    exSpace01     = fmap readSpace <$> lines dat
    coords        = spaceToCoords exSpace01
    dists         = calcAllDistances coords
    (c, answer01) = getMaxDetections $ getAllDetections dists coords
    angs          = coordsWithAngleDist (filter (/=c) coords) dists c
    answer02      = getDestroyingList (Z [] angs) 
  print $ length coords
  print answer01
  print $ answer02 !! 199
