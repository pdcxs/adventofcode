module Year2022.Day15 (solution1, solution2) where

import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

targetLine :: Int
targetLine = 2000000

type Coord = Int
type Pos = (Coord, Coord)
type Rng = (Coord, Coord)

processInput :: String -> [(Pos, Pos)]
processInput = map parse . lines
 where
  parse s =
    let nums = getNums s
     in ((head nums, nums !! 1), (nums !! 2, nums !! 3))
  getNums = map readNum . extractCoords
  readNum =
    read
      . takeWhile (\c -> isDigit c || c == '-')
      . drop 2
  extractCoords = filter isPos . words
  isPos x = head x == 'x' || head x == 'y'

getBeaconCoord :: Int -> [(Pos, Pos)] -> [Coord]
getBeaconCoord _ [] = []
getBeaconCoord ln ((_, (x, y)) : ps)
  | y == ln = x : getBeaconCoord ln ps
  | otherwise = getBeaconCoord ln ps

getSensorRng :: Int -> [(Pos, Pos)] -> [Rng]
getSensorRng _ [] = []
getSensorRng ln (((sx, sy), (bx, by)) : ps)
  | dy <= dist =
      (sx - dx, sx + dx) : getSensorRng ln ps
  | otherwise = getSensorRng ln ps
 where
  dy = abs (ln - sy)
  dist = abs (sx - bx) + abs (sy - by)
  dx = dist - dy

mergeRanges :: [Rng] -> [Rng]
mergeRanges = merge . sort
 where
  merge [] = []
  merge [x] = [x]
  merge ((a1, a2) : (b1, b2) : ps)
    | a2 < b1 = (a1, a2) : merge ((b1, b2) : ps)
    | a2 > b2 = merge ((a1, a2) : ps)
    | otherwise = merge ((a1, b2) : ps)

countPos :: [Rng] -> [Coord] -> Int
countPos rngs beacons = go 0 rngs (sort beacons)
 where
  go c [] _ = c
  go c rs [] = c + sum (map countRngPos rs)
  go c ((x, y) : rs) bs =
    let bs' = dropWhile (< x) bs
        (inRng, bs'') = span (<= y) bs'
        c' = countRngPos (x, y) - length inRng + c
     in go c' rs bs''
  countRngPos (x, y) = y - x + 1

solution1 :: String -> IO ()
solution1 s = print $ countPos rngs beacons
 where
  processed = processInput s
  rngs = mergeRanges $ getSensorRng targetLine processed
  beacons = nubOrd $ getBeaconCoord targetLine processed

-- final coord must be determined by four sensors
-- its distance to four sensors is larger by one
-- than their corresponding dist to beacons
-- the points that has distance larger by 1 to a sensor
-- to its beacon dist make the edge of that sensor.
-- we should find these sensors: their edges are same line
-- assume: (sx, sy) (bx, by)
-- dist: abs (sx - bx) + abs (sy - by) + 1
-- lines: abs (x - sx) + abs (y - sy) = dist
-- line1: x + y = dist + sx + sy
-- line2: x - y = dist + sx - sy
-- line3: x - y = sx - sy - dist
-- line4: x + y = sx + sy - dist
-- find all lines that has two sensors
-- get their intersections and examine them

-- Line: Coefficient of y and right side num
-- coefficient of x is always 1
type Line = (Int, Int)

getEdge :: (Pos, Pos) -> [Line]
getEdge ((sx, sy), (bx, by)) =
  [ (1, dist + sx + sy)
  , (-1, dist + sx - sy)
  , (-1, sx - sy - dist)
  , (1, sx + sy - dist)
  ]
 where
  dist = abs (sx - bx) + abs (sy - by) + 1

getCoord :: Line -> Line -> Maybe Pos
getCoord (cy1, b1) (cy2, b2)
  | cy1 == cy2 = Nothing
  | otherwise = Just (x, y)
 where
  x = (b1 + b2) `div` 2
  y = cy1 * (b1 - b2) `div` 2

getPossibleLines :: [(Pos, Pos)] -> [Line]
getPossibleLines =
  M.keys
    . M.filter (> (1 :: Int))
    . M.fromListWith (+)
    . map (,1)
    . concatMap getEdge

getIntersections :: [Line] -> [Maybe Pos]
getIntersections = map (uncurry getCoord) . combs
 where
  combs [] = []
  combs (x : xs) = map (x,) xs ++ combs xs

solution2 :: String -> IO ()
solution2 s =
  print
    . (\(x, y) -> x * 4000000 + y)
    . head
    . filter valid
    . catMaybes
    . getIntersections
    . getPossibleLines
    $ processed
 where
  processed = processInput s
  valid (x, y) =
    let f a = a >= 0 && a <= 4000000
     in f x && f y && all (illegal (x, y)) processed
  illegal (x, y) ((sx, sy), (bx, by)) =
    abs (x - sx) + abs (y - sy)
      > abs (sx - bx) + abs (sy - by)
