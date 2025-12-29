module Year2022.Day18 (solution1, solution2) where

import Data.List.Split (splitOn)
import qualified Data.Set as S

type Pos = (Int, Int, Int)

processInput :: String -> S.Set Pos
processInput = S.fromList . map parsePoint . lines
 where
  parsePoint s =
    let nums = map read (splitOn "," s)
     in (head nums, nums !! 1, last nums)

getNeighbors :: Pos -> [Pos]
getNeighbors (x, y, z) =
  [ (x - 1, y, z)
  , (x + 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z + 1)
  , (x, y, z - 1)
  ]

solution1 :: String -> IO ()
solution1 s = print $ length ns
 where
  ps = processInput s
  ns =
    concatMap (filter (`S.notMember` ps) . getNeighbors) ps

getOutsides :: S.Set Pos -> S.Set Pos
getOutsides ps = search S.empty (S.singleton start)
 where
  getMaxMin f =
    let cs = S.map f ps
     in (S.findMin cs, S.findMax cs)
  xr@(minx, _) = getMaxMin (\(x, _, _) -> x)
  yr@(miny, _) = getMaxMin (\(_, y, _) -> y)
  zr@(minz, _) = getMaxMin (\(_, _, z) -> z)
  start = (minx - 1, miny - 1, minz - 1)
  inRng x (l, r) = x >= l - 1 && x <= r + 1
  isValid (x, y, z) =
    inRng x xr && inRng y yr && inRng z zr
  search visited newNs
    | S.null newNs = visited
    | otherwise = search visited' newNs'
   where
    visited' = S.union visited newNs
    newNs' =
      let ns =
            S.fromList $
              concatMap (filter isValid . getNeighbors) newNs
       in S.difference (S.difference ns visited') ps

solution2 :: String -> IO ()
solution2 s = print $ length ns
 where
  ps = processInput s
  outside = getOutsides ps
  ns =
    concatMap
      ( filter (\p -> S.notMember p ps && S.member p outside)
          . getNeighbors
      )
      ps
