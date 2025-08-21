module Year2023.Day21 (
 solution1,
 solution2,
) where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as S

type Pos = (Int, Int)

-- positions of walls
type Map = S.Set Pos

processInput :: String -> (Int, Int, Pos, Map)
processInput input = (w, h, pos, walls)
 where
  ls = lines input
  w = length (head ls)
  h = length ls
  (pos, walls) = parseMap 0 0 [] S.empty ls

parseMap ::
 Int ->
 Int ->
 [Pos] ->
 Map ->
 [String] ->
 (Pos, Map)
parseMap _ _ pos walls [] = (head pos, walls)
parseMap _ y pos walls ([] : ls) =
 parseMap 0 (y + 1) pos walls ls
parseMap x y pos walls (('#' : cs) : ls) =
 let walls' = S.insert (x, y) walls
  in parseMap (x + 1) y pos walls' (cs : ls)
parseMap x y _ walls (('S' : cs) : ls) =
 parseMap (x + 1) y [(x, y)] walls (cs : ls)
parseMap x y pos walls ((_ : cs) : ls) =
 parseMap (x + 1) y pos walls (cs : ls)

getCandidates :: Int -> Int -> Map -> Int -> [Pos] -> [Pos]
getCandidates _ _ _ 0 pos = pos
getCandidates w h walls n pos =
 let next =
      concatMap
       ( \(x, y) ->
          [ (x + 1, y)
          , (x - 1, y)
          , (x, y + 1)
          , (x, y - 1)
          ]
       )
       pos
     validNext =
      nubOrd $
       filter
        ( \(x, y) ->
           S.notMember
            (x `mod` w, y `mod` h)
            walls
        )
        next
  in getCandidates w h walls (n - 1) validNext

solution1 :: String -> IO ()
solution1 input =
 print . length $
  (getCandidates w h walls 305 [start])
 where
  (w, h, start, walls) = processInput input

-- 26501365 = 202300 * 131 + 65
-- and grid width and height is 131
-- if result is f(n)
-- then let g(x) = f(x * 131 + 65)
-- g(x) will be a quadratic function
-- let g(x) = a * x^2 + b * x + c
-- let solve a, b and c
-- and calculate g(202300)
-- this is a little bit slow because
-- we iterate all possible locations in
-- problem 1, we can optimize it
-- by consider the distance is odd or even
-- but this solution is very simple
-- and I love it, so I will keep it.
solution2 :: String -> IO ()
solution2 input = print $ g 202300
 where
  (w, h, start, walls) = processInput input
  y0 = length $ getCandidates w h walls 65 [start]
  y1 = length $ getCandidates w h walls (65 + 131) [start]
  y2 = length $ getCandidates w h walls (65 + 262) [start]
  -- 0a + 0b + c == y0
  -- a + b + c == y1
  -- 4a + 2b + c == y2
  c = y0
  a = (y2 - 2 * y1 + c) `div` 2
  b = y1 - a - c
  g x = a * x * x + b * x + c