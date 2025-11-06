module Year2022.Day09 (solution1, solution2) where

import qualified Data.Set as S

type Pos = (Int, Int)
type Dir = (Int, Int)

parseInput :: String -> [Dir]
parseInput = map getDir . lines
 where
  getDir ('R' : ' ' : s) = (read s, 0)
  getDir ('L' : ' ' : s) = (-read s, 0)
  getDir ('U' : ' ' : s) = (0, read s)
  getDir ('D' : ' ' : s) = (0, -read s)
  getDir _ = undefined

moveHead :: Pos -> [Dir] -> (Pos, [Dir])
moveHead p [] = (p, [])
moveHead p ((0, 0) : ds) = moveHead p ds
moveHead (x, y) ((0, dy) : ds)
  | dy > 0 =
      ((x, y + 1), (0, dy - 1) : ds)
  | otherwise =
      ((x, y - 1), (0, dy + 1) : ds)
moveHead (x, y) ((dx, 0) : ds)
  | dx > 0 =
      ((x + 1, y), (dx - 1, 0) : ds)
  | otherwise =
      ((x - 1, y), (dx + 1, 0) : ds)
moveHead _ _ = undefined

pull :: Pos -> Pos -> Pos
pull h@(hx, hy) t@(tx, ty) =
  if isNeighbor h t
    then t
    else
      ( tx + sign (hx - tx)
      , ty + sign (hy - ty)
      )
 where
  sign x
    | x > 0 = 1
    | x < 0 = -1
    | otherwise = 0

run ::
  [Pos] -> S.Set Pos -> [Dir] -> S.Set Pos
run (h : ns) visited dirs
  | null dirs' = visited
  | otherwise = run ns' visited' dirs'
 where
  (h', dirs') = moveHead h dirs
  ns' = go h' ns
  go hd [] = [hd]
  go hd (t : ts) = hd : go (pull hd t) ts
  visited' = S.insert (last ns') visited
run _ _ _ = undefined

isNeighbor :: Pos -> Pos -> Bool
isNeighbor (x1, y1) (x2, y2) =
  abs (x1 - x2) <= 1
    && abs (y1 - y2) <= 1

solve :: Int -> String -> IO ()
solve n =
  print
    . S.size
    . run (replicate n (0, 0)) S.empty
    . parseInput

solution1 :: String -> IO ()
solution1 = solve 2

solution2 :: String -> IO ()
solution2 = solve 10
