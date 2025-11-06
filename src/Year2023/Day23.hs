module Year2023.Day23 (solution1, solution2) where

import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

type Pos = (Int, Int)
type Map = M.Map Pos Char

processInput ::
  String -> (Int, Int, Map)
processInput input = (w, h, m)
 where
  ls = lines input
  h = length ls
  w = length (head ls)
  m =
    M.fromList
      [ ((x, y), c)
      | (y, row) <- zip [0 ..] ls
      , (x, c) <- zip [0 ..] row
      ]

neighbors ::
  Map ->
  Pos ->
  [Pos]
neighbors m (x, y) =
  case c of
    '#' -> []
    '>' -> candidates [(x + 1, y)]
    'v' -> candidates [(x, y + 1)]
    '<' -> candidates [(x - 1, y)]
    '^' -> candidates [(x, y - 1)]
    _ ->
      candidates
        [ (x - 1, y)
        , (x + 1, y)
        , (x, y - 1)
        , (x, y + 1)
        ]
 where
  c = m M.! (x, y)
  candidates = filter (isValid m)

isValid :: Map -> Pos -> Bool
isValid m p = case M.lookup p m of
  Just c -> c /= '#'
  Nothing -> False

junctions ::
  Map -> Pos -> Pos -> S.Set Pos
junctions m start end =
  S.insert start
    . S.insert end
    $ S.fromList
      [ p
      | p <- M.keys m
      , m M.! p /= '#'
      , length (neighbors m p) > 2
      ]

-- build an undirected junction graph
-- to optimize the search space
buildGraph ::
  Map ->
  S.Set Pos ->
  M.Map Pos [(Pos, Int)]
buildGraph m js =
  M.fromList
    [(j, findConns j) | j <- S.toList js]
 where
  -- get all neighbor junctions of given position
  findConns j =
    catMaybes
      [walk j n 1 | n <- neighbors m j]
  -- get next junction
  -- in (prev, cur) direction
  walk ::
    Pos ->
    Pos ->
    Int ->
    Maybe (Pos, Int)
  walk prev curr dist
    | S.member curr js = Just (curr, dist)
    | otherwise =
        if length nexts /= 1
          then Nothing
          else walk curr (head nexts) (dist + 1)
   where
    nexts = filter (/= prev) (neighbors m curr)

longestPath ::
  M.Map Pos [(Pos, Int)] ->
  Pos ->
  Pos ->
  Int
longestPath g start end =
  go (S.singleton start) start 0
 where
  -- all possible paths from cur to end
  go :: S.Set Pos -> Pos -> Int -> Int
  go visited curr steps
    | curr == end = steps
    | otherwise =
        if null cands
          then 0
          else maximum cands
   where
    cands =
      [ go
          (S.insert next visited)
          next
          (steps + dist)
      | (next, dist) <- g M.! curr
      , S.notMember next visited
      ]

solution ::
  (String -> String) -> String -> IO ()
solution process input =
  print $ longestPath g start end
 where
  (w, h, m) = processInput (process input)
  start = (1, 0)
  end = (w - 2, h - 1)
  js = junctions m start end
  g = buildGraph m js

solution1 :: String -> IO ()
solution1 = solution id

solution2 :: String -> IO ()
solution2 = solution preProcess
 where
  preProcess [] = []
  preProcess (c : cs)
    | c `elem` ">v<^" = '.' : preProcess cs
    | otherwise = c : preProcess cs
