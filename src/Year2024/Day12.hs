module Year2024.Day12 (solution1, solution2) where

import Data.List (unfoldr)
import qualified Data.Map as M
import qualified Data.Set as S

type Loc = (Int, Int)

type Map = M.Map Loc Char

type Dir = (Int, Int)

type Region = S.Set Loc

processInput :: String -> [Region]
processInput s = regions m
 where
  ls = lines s
  m = parseMap 0 0 ls M.empty

parseMap :: Int -> Int -> [String] -> Map -> Map
parseMap _ _ [] m = m
parseMap _ y ([] : ss) m = parseMap 0 (y + 1) ss m
parseMap x y ((c : cs) : ss) m =
  parseMap
    (x + 1)
    y
    (cs : ss)
    (M.insert (x, y) c m)

-- reverse version of foldr
-- start from total map
-- each time, get a new map and a region
-- when map is empty, we collect all regions
regions :: Map -> [Region]
regions = unfoldr go
 where
  go m = case M.lookupMin m of
    Just (loc, c) -> getRegion c S.empty [loc] m
    Nothing -> Nothing

-- get current region
-- Char is current region label
-- S.Set Loc is visited locations
-- [Loc] is candidate locations
-- Map is original map
-- return obtained region and new map
-- that removed region locations
getRegion :: Char -> S.Set Loc -> [Loc] -> Map -> Maybe (Region, Map)
getRegion c visited candidates m
  -- if there's no locations to be explored, then we are done
  | null candidates = Just (visited, m')
  | otherwise = getRegion c visited' ls' m
 where
  m' = M.withoutKeys m visited -- remove locations in current region
  l = head candidates -- pick up first candidates
  ls = tail candidates
  visited' = S.insert l visited
  next =
    -- generate new locations should be explored
    [ loc
    | loc <- neighbors l -- it is neighbor of l
    , loc `S.notMember` visited -- has not been visited
    , m M.!? loc == Just c -- has same label
    , loc `notElem` ls -- not in candidates
    ]
  ls' = next ++ ls -- add new locations to candidates

neighbors :: Loc -> [Loc]
neighbors (x, y) =
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]

-- an edge block is a location and a direction
-- that points to its corresponding location
-- in the region.
edges :: Region -> [(Loc, Dir)]
edges r =
  [ ((ex, ey), (x - ex, y - ey))
  | (x, y) <- S.toList r
  , (ex, ey) <- neighbors (x, y)
  , (ex, ey) `S.notMember` r
  ]

solution1 :: String -> String
solution1 s = show . sum $ zipWith (*) crs ces
 where
  rgs = processInput s
  edgs = map edges rgs
  crs = map S.size rgs
  ces = map length edgs

solution2 :: String -> String
solution2 s = show . sum $ zipWith (*) crns areas
 where
  rgs = processInput s
  crns = map (length . corners) rgs
  areas = map S.size rgs

-- corner count equals to side count
-- edge location go to turn right of direction
-- should not be in the edges.
-- notice: direction also need to be considered
corners :: Region -> [Loc]
corners r =
  [ (x + dx, y + dy)
  | ((x, y), (dx, dy)) <- edgs
  , ((x - dy, y + dx), (dx, dy)) `notElem` edgs
  ]
 where
  edgs = edges r
