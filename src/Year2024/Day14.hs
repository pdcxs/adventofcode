module Year2024.Day14 (solution1, solution2) where

import Data.List.Split (splitOneOf)
import qualified Data.Set as S

type Pos = (Int, Int)

type Dir = (Int, Int)

processInput :: String -> [(Pos, Dir)]
processInput s = map (getData . getNums) ls
 where
  ls = lines s
  getNums = map read . filter (not . null) . splitOneOf "p=,v "
  getData xs = ((head xs, xs !! 1), (xs !! 2, xs !! 3))

width :: Int
-- width = 11 -- for test
width = 101

height :: Int
-- height = 7 -- for test
height = 103

getLoc :: Int -> (Pos, Dir) -> Pos
getLoc time ((x, y), (dx, dy)) = (x', y')
 where
  x' = (x + dx * time) `mod` width
  y' = (y + dy * time) `mod` height

midWidth :: Int
midWidth = width `div` 2

midHeight :: Int
midHeight = height `div` 2

collect :: Pos -> [Int] -> [Int]
collect (x, y) acc
  | x == midWidth || y == midHeight = acc
  | x < midWidth && y < midHeight = zipWith (+) q1 acc
  | x > midWidth && y < midHeight = zipWith (+) q2 acc
  | x < midWidth && y > midHeight = zipWith (+) q3 acc
  | otherwise = zipWith (+) q4 acc
 where
  q1 = [1, 0, 0, 0]
  q2 = [0, 1, 0, 0]
  q3 = [0, 0, 1, 0]
  q4 = [0, 0, 0, 1]

solution1 :: String -> String
solution1 =
  show
    . product
    . foldr (collect . getLoc 100) [0, 0, 0, 0]
    . processInput

-- an ester egg should have no overlaps
isOverlap :: S.Set Pos -> [Pos] -> Bool
isOverlap _ [] = True
isOverlap s (p : ps) =
  not (p `S.member` s)
    && isOverlap (S.insert p s) ps

printMap :: [Pos] -> String
printMap pos = go 0 0 []
 where
  go x y ss
    | x == width = go 0 (y + 1) ('\n' : ss)
    | y == height = reverse ss
    | otherwise =
        if (x, y) `S.member` loc
          then go (x + 1) y ('X' : ss)
          else go (x + 1) y (' ' : ss)
  loc = S.fromList pos

solution2 :: String -> String
solution2 s = show time ++ "\n" ++ printMap (head r)
 where
  inputs = processInput s
  locs = map (\t -> map (getLoc t) inputs) [1 ..]
  (overlaps, r) = break (isOverlap S.empty) locs
  time = length overlaps + 1
