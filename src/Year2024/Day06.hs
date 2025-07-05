module Year2024.Day06 (solution1, solution2) where

import Data.Array.Unboxed (
  Array,
  amap,
  array,
  assocs,
  (!?),
  (//),
 )
import Data.Containers.ListUtils (nubOrd)

type Pos = (Int, Int)

type Dir = (Int, Int)

processInput :: String -> Array Pos Char
processInput s = array ((0, 0), (w - 1, h - 1)) grid
 where
  ls = lines s
  w = length (head ls)
  h = length ls
  grid = getGrid 0 0 ls
  getGrid _ _ [] = []
  getGrid _ y ([] : css) = getGrid 0 (y + 1) css
  getGrid x y ((c : cs) : css) =
    ((x, y), c)
      : getGrid (x + 1) y (cs : css)

solution1 :: String -> String
solution1 s = show $ length $ nubOrd path
 where
  grid = processInput s
  start = head [p | (p, '^') <- assocs grid]
  obs = amap (== '#') grid
  path = map fst $ walk obs start (0, -1)

walk :: Array (Int, Int) Bool -> Pos -> Dir -> [(Pos, Dir)]
walk obs pos@(x, y) dir@(dx, dy) =
  (pos, dir)
    : case obs !? np of
      Nothing -> []
      Just True -> walk obs pos (turnRight dir)
      Just False -> walk obs np dir
 where
  np = (x + dx, y + dy)

turnRight :: Dir -> Dir
turnRight (dx, dy) = (-dy, dx)

solution2 :: String -> String
solution2 s =
  show $ length $ filter isLoop newPaths
 where
  grid = processInput s
  start = head [p | (p, '^') <- assocs grid]
  obs = amap (== '#') grid
  path = nubOrd $ tail $ map fst $ walk obs start (0, -1)
  newObs = map (\p -> obs // [(p, True)]) path
  newPaths = map (\o -> walk o start (0, -1)) newObs

-- see here
-- https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare
isLoop :: [(Pos, Dir)] -> Bool
isLoop stats = go stats stats
 where
  go (x : xs) (_ : y : ys) = x == y || go xs ys
  go _ _ = False
