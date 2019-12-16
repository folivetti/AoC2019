module Day02 where

import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map as M

data Expr = Const Int
          | Var String
          | Add Expr Expr
          | Mul Expr Expr

instance Show Expr where
  show (Const x) = show x
  show (Var s)   = s
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  
-- | Day 02
examples2 = [1,9,10,3,2,3,11,0,99,30,40,50]
codes     = chunksOf 4 examples2

applyRule :: [Int] -> [Int] -> [Int]
applyRule xs [1, i, j, k] =  take k xs ++ x' : drop (k+1) xs
  where x' = (xs!!i) + (xs!!j)
applyRule xs [2, i, j, k] =  take k xs ++ x' : drop (k+1) xs
  where x' = (xs!!i) * (xs!!j)
applyRule xs _ = xs

simplify :: Expr -> Expr
simplify (Const x) = Const x
simplify (Var s)   = Var s
simplify (Add (Const x) (Const y)) = Const (x+y)
simplify (Add (Const x) (Add (Const y) e)) = Add (Const (x+y)) (simplify e)
simplify (Add (Const x) (Add e (Const y))) = Add (Const (x+y)) (simplify e)
simplify (Mul (Const x) (Const y)) = Const (x*y)
simplify (Mul (Const x) (Add (Const y) e)) = Add (Const (x*y)) (Mul (Const x) (simplify e))
simplify (Mul (Add (Const y) e) (Const x)) = Add (Const (x*y)) (Mul (Const x) (simplify e))
simplify (Mul (Const x) (Mul e (Const y))) = Mul (Const (x*y)) (simplify e) 
simplify (Mul (Const x) (Mul (Const y) e)) = Mul (Const (x*y)) (simplify e) 
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify (Mul e1 e2) = Mul (simplify e1) (simplify e2)

applyRule' :: [Expr] -> [Int] -> [Expr]
applyRule' es [1, i, j, k] = take k es ++ e' : drop (k+1) es
  where e' = simplify $ Add (es!!i) (es!!j)
applyRule' es [2, i, j, k] = take k es ++ e' : drop (k+1) es
  where e' = simplify $ Mul (es!!i) (es!!j)
applyRule' es _ = es

compute :: [Int] -> [Int]
compute xs = foldl applyRule xs (chunksOf 4 xs)

compute' :: [Int] -> [Expr]
compute' code@(x:y:z:xs) = foldl applyRule' es (chunksOf 4 code)
  where es = Const x : Var "x" : Var "y" : fmap Const xs

replaceCode :: Int -> Int -> [Int] -> [Int]
replaceCode noun verb (x:y:z:xs) = x : noun : verb : xs

findOutput :: Int -> [Int] -> (Int, Int)
findOutput x xs = (head . filter isX) ops
  where range = [0..99]
        ops   = [(n, v) | n <- range, v <- range]
        isX = (==x) . head . compute . (flip. uncurry) replaceCode xs

day02 :: IO ()
day02 = do
  day02 <- replaceCode 12 2 . fmap read . splitOn "," <$> readFile "day02.txt"

  print $ foldl applyRule examples2 codes
  print $ head $ compute day02
  print $ findOutput 19690720 day02

  print $ simplify . head $ compute' day02
