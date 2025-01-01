module Year2024.Day4 (solution1, solution2) where

import qualified Data.Vector as V

type Grid = V.Vector (V.Vector Char)

type Pos = (Int, Int) -- Row Col

processInput :: String -> Grid
processInput = V.fromList . map V.fromList . lines

inGrid :: Grid -> Pos -> Bool
inGrid grid (r, c) =
  r >= 0 && r < height && c >= 0 && c < width
  where
    (width, height) = getSize grid

getSize :: Grid -> (Int, Int)
getSize grid = (V.length (V.head grid), V.length grid)

posStrCount :: Grid -> Pos -> Int
posStrCount grid (r, c) = length $ filter isXMAS [hs, vs, ds, ds']
  where
    n = [0 .. 3]
    h = map (\i -> (r, c + i)) n
    v = map (\i -> (r + i, c)) n
    d = map (\i -> (r + i, c + i)) n
    d' = map (\i -> (r + i, c - i)) n
    hs = map (get grid) $ filter (inGrid grid) h
    vs = map (get grid) $ filter (inGrid grid) v
    ds = map (get grid) $ filter (inGrid grid) d
    ds' = map (get grid) $ filter (inGrid grid) d'

isXMAS :: String -> Bool
isXMAS s = s == "XMAS" || s == "SAMX"

get :: Grid -> Pos -> Char
get grid (r, c) = grid V.! r V.! c

solution1 :: String -> String
solution1 s =
  show $
    sum $
      [ posStrCount grid (r, c)
        | r <- [0 .. height - 1],
          c <- [0 .. width - 1]
      ]
  where
    grid = processInput s
    (width, height) = getSize grid

solution2 :: String -> String
solution2 s =
  show $
    length $
      filter
        (isXShapedMAS grid)
        [ (r, c)
          | r <- [0 .. height - 1],
            c <- [0 .. width - 1]
        ]
  where
    grid = processInput s
    (width, height) = getSize grid

isXShapedMAS :: Grid -> Pos -> Bool
isXShapedMAS grid pos@(r, c) =
  get grid pos == 'A'
    && c1
    && c2
    && c3
    && c4
  where
    ps =
      [ (r - 1, c - 1),
        (r + 1, c - 1),
        (r - 1, c + 1),
        (r + 1, c + 1)
      ]
    c1 = all (inGrid grid) ps
    s = map (get grid) ps
    c2 = head s /= last s
    c3 = length (filter (== 'M') s) == 2
    c4 = length (filter (== 'S') s) == 2
