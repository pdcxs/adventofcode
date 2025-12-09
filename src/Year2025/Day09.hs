module Year2025.Day09 (solution1, solution2) where

import Data.List (sort)
import Data.List.Split (splitOn)

type Pos = (Int, Int)

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

-- We can find right most inner points
-- split the shape into top parts and bottom parts
-- test each point with the corresponding inner point
-- filter out the rects contains other points
-- and get the maximum area.

findInner :: [Pos] -> [Pos]
findInner xs =
  sort
    . map (uncurry max)
    . filter
      ( \((x1, _), (x2, _)) ->
          abs (x1 - x2) > 50000
      )
    $ zip xs (tail xs)

validPair :: [Pos] -> (Pos, Pos) -> Bool
validPair ps rect = not $ any (inRect rect) ps
 where
  inRect ((x1, y1), (x2, y2)) (x, y) =
    min x1 x2 < x
      && max x1 x2 > x
      && min y1 y2 < y
      && max y1 y2 > y

solution2 :: String -> IO ()
solution2 s = print $ max as1 as2
 where
  pos = processInput s
  keys = findInner pos
  p1@(_, y1) = head keys -- less y means top inner point
  p2@(_, y2) = last keys -- bottom inner point
  tops = filter ((<= y1) . snd) pos -- top half shape
  bottoms = filter ((>= y2) . snd) pos
  ps1 = filter (validPair tops) $ map (p1,) tops
  ps2 = filter (validPair bottoms) $ map (p2,) bottoms
  as1 = maximum (map (uncurry getRectArea) ps1)
  as2 = maximum (map (uncurry getRectArea) ps2)
