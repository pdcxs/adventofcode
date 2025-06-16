module Year2024.Day08 (solution1, solution2) where

import Common.Utils (safeHead)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M

type Pos = (Int, Int)

type Map = M.Map Char [Pos]

processInput :: String -> (Int, Int, Map)
processInput s = (width, height, m)
 where
  ls = lines s
  height = length ls
  width = length (safeHead ls)
  m = parseMap ls 0 0 M.empty

parseMap ::
  [String] ->
  Int ->
  Int ->
  Map ->
  Map
parseMap [] _ _ m = m
parseMap ([] : ss) _ y m = parseMap ss 0 (y + 1) m
parseMap ((c : cs) : ss) x y m
  | c == '.' = parseMap (cs : ss) (x + 1) y m
  | otherwise = parseMap (cs : ss) (x + 1) y m'
 where
  m' = M.insertWith (++) c [(x, y)] m

antiNodes :: Int -> Int -> [Pos] -> [Pos]
antiNodes w h ps =
  filter
    (inGrid w h)
    [ antiNode p1 p2
    | p1 <- ps
    , p2 <- ps
    , p1 /= p2
    ]

inGrid :: Int -> Int -> Pos -> Bool
inGrid w h (x, y) =
  x >= 0
    && x < w
    && y >= 0
    && y < h

antiNode :: Pos -> Pos -> Pos
antiNode (x1, y1) (x2, y2) =
  (2 * x2 - x1, 2 * y2 - y1)

solution1 :: String -> String
solution1 s = show $ length $ nubOrd ps
 where
  (w, h, m) = processInput s
  ps = concatMap (antiNodes w h) $ M.elems m

solution2 :: String -> String
solution2 s =
  show $
    length $
      nubOrd $
        concatMap (getAntiPos w h) ps
 where
  (w, h, m) = processInput s
  ps = M.elems m

getAntiPos :: Int -> Int -> [Pos] -> [Pos]
getAntiPos w h ps =
  concat
    [ getAntiNodes' w h p1 p2
    | p1 <- ps
    , p2 <- ps
    , p1 /= p2
    ]

getAntiNodes' :: Int -> Int -> Pos -> Pos -> [Pos]
getAntiNodes' w h (x1, y1) (x2, y2) =
  takeWhile (inGrid w h) (map get [-1, -2 ..])
    ++ takeWhile (inGrid w h) (map get [0 ..])
 where
  dx = x2 - x1
  dy = y2 - y1
  g = gcd dx dy
  dx' = dx `div` g
  dy' = dy `div` g
  get n = (x1 + n * dx', y1 + n * dy')
