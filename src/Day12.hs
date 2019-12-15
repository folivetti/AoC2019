module Day12 where

data Coord = C { x :: Int
               , y :: Int
               , z :: Int
               } deriving (Show, Eq)

data Moon = M { pos :: Coord, vel :: Coord } 
                deriving (Show, Eq)

zeroCoord = C 0 0 0
tripleToCoord (x,y,z) = C x y z

sumCoord (C x y z) (C a b c) = C (x+a) (y+b) (z+c)

moons = [M (tripleToCoord (-6,2,-9)) zeroCoord
        ,M (tripleToCoord (12,-14,-4)) zeroCoord
        ,M (tripleToCoord (9,5,-6)) zeroCoord
        ,M (tripleToCoord (-1,-4,9)) zeroCoord
        ]

moonE = [M (tripleToCoord (-1,0,2)) zeroCoord
        ,M (tripleToCoord (2,-10,-7)) zeroCoord
        ,M (tripleToCoord (4,-8,8)) zeroCoord
        ,M (tripleToCoord (3,5,-1)) zeroCoord
        ]

timeStep :: [Moon] -> [Moon]
timeStep ms = fmap updatePos ms'
  where
    ms' = fmap (updateVel ms) ms

updatePos :: Moon -> Moon
updatePos m@(M p v) = m{ pos = sumCoord p v }

updateVel :: [Moon] -> Moon -> Moon
updateVel ms m = foldl update m ms
  where
    update m1@(M p1 v1) m2@(M p2 v2) = m1{ vel = sumCoord v1 (delta p1 p2) }
    delta (C x y z) (C a b c) = C (f x a) (f y b) (f z c)
    f x y | x < y = 1
          | x > y = -1
          |otherwise = 0

potentialEnergy, kineticEnergy, totalEnergy :: Moon -> Int

potentialEnergy (M (C x y z) _) = abs x + abs y + abs z
kineticEnergy   (M _ (C x y z)) = abs x + abs y + abs z
totalEnergy m                   = potentialEnergy m * kineticEnergy m

totalSystemEnergy :: [Moon] -> Int
totalSystemEnergy = sum . fmap totalEnergy

findCycle :: [Moon] -> (Coord -> Int) -> Int
findCycle ms getC = (2*) . (+1) . length $ takeWhile allNonZero $ tail $ iterate timeStep ms
  where
    allNonZero ms' = or (fmap ((/=0) . getC . vel) ms')

day12 :: IO ()
day12 = do
  let
    simulation = iterate timeStep moons
    cycleX     = findCycle moons x
    cycleY     = findCycle moons y
    cycleZ     = findCycle moons z
  print $ totalSystemEnergy (simulation !! 1000)
  print $ lcm cycleZ $ lcm cycleX cycleY
