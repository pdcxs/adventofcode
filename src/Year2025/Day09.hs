module Year2025.Day09 (solution1, solution2) where

-- the input shape looks like:
--      XXXXXXX
--    XXXXXXXXXXXXX
--   XXXXXXXXXXXXXXX
--  XXXXXXXXXXXXXXXXX
-- XXXXXXXXXXXXXXXXXXX
--                  XX
-- XXXXXXXXXXXXXXXXXXX
--  XXXXXXXXXXXXXXXXX
--   XXXXXXXXXXXXXXX
--    XXXXXXXXXXXXX
--       XXXXXXX
--
-- So we don't need to worry about too complex cases
-- For example
-- In following shape:
--
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXX                    XXX
-- XXXXXXXXXXXXXXXXXXXXXXXXXX
-- XXXXXXXXXXXXXXXXXXXXXXXXXX
--
-- we need to worry about 4 corners of the hole
-- There is no lines cross it but it's not valid
-- However, this input doesn't include this case

import Data.Foldable (maximumBy)
import Data.List.Split (splitOn)

type Pos = (Int, Int)
type Line = (Pos, Pos)
type Rect = (Pos, Pos)

processInput :: String -> [Pos]
processInput = map parse . lines
 where
  parse xs =
    let ss = splitOn "," xs
     in (read (head ss), read (last ss))

getRectArea :: Pos -> Pos -> Int
getRectArea (x, y) (x', y') =
  (1 + abs (x - x')) * (1 + abs (y - y'))

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs (x : xs) = map (x,) xs ++ getPairs xs

solution1 :: String -> IO ()
solution1 =
  print
    . maximum
    . map (uncurry getRectArea)
    . getPairs
    . processInput

rectValid :: [Line] -> Rect -> Bool
rectValid edges rect = not $ any (check rect) edges

check :: Rect -> Line -> Bool
check ((lx, by), (rx, ty)) ((x1, y1), (x2, y2))
  | x1 == x2 =
      inRange x1 (lx, rx)
        && overlap (by, ty) (min y1 y2, max y1 y2)
  | otherwise =
      inRange y1 (by, ty)
        && overlap (lx, rx) (min x1 x2, max x1 x2)
 where
  inRange v (a, b) = a < v && b > v
  overlap (a1, b1) (a2, b2) =
    not (b1 <= a2 || a1 >= b2)

solution2 :: String -> IO ()
solution2 input =
  print $
    maximumBy (\(a1, _) (a2, _) -> compare a1 a2) $
      zip areas ps
 where
  pos = processInput input
  edges = (last pos, head pos) : zip pos (tail pos)
  pairs = map fix $ getPairs pos
  ps = filter (rectValid edges) pairs
  areas = map (uncurry getRectArea) ps
  fix ((x1, y1), (x2, y2)) =
    ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
