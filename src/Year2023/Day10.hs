module Year2023.Day10 (solution1, solution2) where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)

type Pos = (Int, Int)
type Pipe = M.Map Pos Pos
type Map = M.Map Pos Pipe

processInput :: String -> (Pos, Map)
processInput = parseMap 0 0 ((0, 0), M.empty)

parseMap :: Int -> Int -> (Pos, Map) -> String -> (Pos, Map)
parseMap _ _ rcd [] = rcd
parseMap x y (_, m) ('S' : ss) =
  parseMap (x + 1) y ((x, y), (M.insert (x, y) M.empty m)) ss
parseMap _ y rcd ('\n' : ss) =
  parseMap 0 (y + 1) rcd ss
parseMap x y (p, m) (c : ss) =
  parseMap (x + 1) y (p, M.insert (x, y) (getPipe c x y) m) ss

getPipe :: Char -> Int -> Int -> Pipe
getPipe '|' x y =
  M.fromList
    [ ((x, y - 1), (x, y))
    , ((x, y + 1), (x, y))
    ]
getPipe '-' x y =
  M.fromList
    [ ((x + 1, y), (x, y))
    , ((x - 1, y), (x, y))
    ]
getPipe 'L' x y =
  M.fromList
    [ ((x, y - 1), (x, y))
    , ((x + 1, y), (x, y))
    ]
getPipe 'J' x y =
  M.fromList
    [ ((x, y - 1), (x, y))
    , ((x - 1, y), (x, y))
    ]
getPipe '7' x y =
  M.fromList
    [ ((x, y + 1), (x, y))
    , ((x - 1, y), (x, y))
    ]
getPipe 'F' x y =
  M.fromList
    [ ((x, y + 1), (x, y))
    , ((x + 1, y), (x, y))
    ]
getPipe _ _ _ = M.empty

search :: Map -> Pos -> Pos -> [Pos] -> [Pos]
search m start ps visited
  | null ps' = visited'
  | otherwise = search m start (head ps') visited'
 where
  ps' = nextPos m start visited ps
  visited' = ps : visited

nextPos :: Map -> Pos -> [Pos] -> Pos -> [Pos]
nextPos m start visited p@(x, y) = do
  neighbor <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  let v = if null visited then [start] else [head visited, start]
  guard (neighbor `notElem` v)
  let r = M.lookup neighbor m
  guard (isJust r)
  let pipe = fromJust r
  let curR = M.lookup p m
  guard (isJust curR)
  let curPipe = fromJust curR
  guard (M.null curPipe || neighbor `M.member` curPipe)
  case M.lookup p pipe of
    Nothing -> []
    Just n -> return n

getPath :: String -> [Pos]
getPath s = search m start start []
 where
  (start, m) = processInput s

solution1 :: String -> String
solution1 s = show c
 where
  ps = getPath s
  c = length ps `div` 2

getArea :: [Pos] -> Double
getArea ps = (abs p - n) / 2 + 1
 where
  cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1
  n = fromIntegral $ length ps
  p = fromIntegral $ sum (zipWith cross ps (drop 1 $ cycle ps))

solution2 :: String -> String
solution2 = show . getArea . getPath
