{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.List (sortOn, groupBy)
import qualified Data.Map.Strict as M
import Data.Char
import Data.Maybe
import Algorithm.Search

type Point2D = (Int, Int)

data Tiles = Portal Point2D| Path
                 deriving Show

type Pluto = M.Map Point2D Tiles

(+:) :: Point2D -> Point2D -> Point2D
(x,y) +: (z,w) = (x+z, y+w)

groupOn f = groupBy (\x y -> f x == f y)

getPaths :: M.Map Point2D Char -> Pluto
getPaths = M.map (const Path) . M.filter (=='.') 

-- | Parsing nightmare
parseFile :: String -> (Pluto, Point2D, Point2D)
parseFile content = (M.union portals paths, entrance, exit)
  where
    paths   = getPaths coords
    portals = (M.fromList . concatMap (merge.map fst) . groupOn snd . init . tail) matchPortals
    
    coords  = M.fromList [((x,y),c) | (x, cs) <- zip [0..] (lines content)
                                    , (y,  c) <- zip [0..] cs]

    
    portalsCoords = M.filter isUpper coords
    matchPortals  = (sortOn snd . mapMaybe getPortals . M.keys) portalsCoords
    entrance      = (fst . head) matchPortals
    exit          = (fst . last) matchPortals
    merge [p1,p2] = [(p1, Portal p2), (p2, Portal p1)] 

    getPortals :: Point2D -> Maybe (Point2D, String)
    getPortals pos | null ps   = Nothing
                   | otherwise = Just (head ps) 
      where
        ps = catMaybes [p1,p2,p3,p4]
        p1 = getPortal' pos (pos +: (0,1))  (pos +: (0,2))
        p2 = getPortal' pos (pos +: (1,0))  (pos +: (2,0))
        p3 = getPortal' pos (pos +: (1,0))  (pos +: (-1,0))
        p4 = getPortal' pos (pos +: (0,1))  (pos +: (0,-1))

        getPortal' :: Point2D -> Point2D -> Point2D -> Maybe (Point2D, String)
        getPortal' a b c | isPortal  = Just (c, name)
                         | otherwise = Nothing
          where
            name = catMaybes [ma, mb]
            ma   = portalsCoords M.!? a
            mb   = portalsCoords M.!? b
            mc   = paths M.!? c
            isPortal = isJust ma && isJust mb && isJust mc

neighborhoodFrom :: Pluto -> Point2D -> [Point2D]
neighborhoodFrom m = filter (`M.member` m) . flip adjacent m

adjacent :: Point2D -> Pluto -> [Point2D]
adjacent pos m = case m M.! pos of
                       Portal to -> to : neighs
                       Path      -> neighs
  where
    neighs = filter (`M.member` m) $ map (pos +:) [(1,0),(0,1),(-1,0),(0,-1)]

neighborsAstar :: Pluto -> Point2D -> Point2D -> (Point2D, Int) -> [(Point2D, Int)]
neighborsAstar m ent ext (p, 0) = case m M.! p of 
                                     Portal to -> if   outer to
                                                  then (to, 1) : neighs
                                                  else neighs
                                     Path      -> neighs
  where
    neighs = map  (,0) $ filter (`M.member` m) $ map (p +:) [(1,0),(0,1),(-1,0),(0,-1)]
    outer (x,y)  = min x y < 5 || x > (maxX - 5) || y > (maxY - 5)
    (maxX, maxY) = foldl (\(x, y) (z, w) -> (max x z, max y w)) (0, 0) $ M.keys m
    
neighborsAstar m ent ext (p, l) = case m M.! p of
                                    Portal to -> if   outer to 
                                                 then (to, l+1) : neighs 
                                                 else (to, l-1) : neighs
                                    Path      -> filter ((`notElem` [ent, ext]).fst) neighs 
  where
    neighs       = map  (,l) $ filter (`M.member` m) $ map (p +:) [(1,0),(0,1),(-1,0),(0,-1)]
    outer (x,y)  = min x y < 5 || x > (maxX - 5) || y > (maxY - 5)
    (maxX, maxY) = foldl (\(x, y) (z, w) -> (max x z, max y w)) (0, 0) $ M.keys m

cost :: a -> a -> Int
cost _ _ = 1

getCost = fst.fromJust

day20 :: IO ()
day20 = do
  dat <- readFile "day20.txt"
  let
    (tiles,entrance,exit) = parseFile dat
  print $ getCost $ dijkstra (neighborhoodFrom tiles) cost (==exit) entrance
  print $ getCost $ aStar (neighborsAstar tiles entrance exit) cost (\(p,l) -> max 0 (l-1)*42) (==(exit,0)) (entrance, 0)
