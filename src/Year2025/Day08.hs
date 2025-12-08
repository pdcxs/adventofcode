module Year2025.Day08 (solution1, solution2) where

import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.List.Split (splitOn)

type Trees = M.IntMap Int
type Pos = (Int, Int, Int)
type DistRecord = M.IntMap [(Int, Int)]

search :: Trees -> Int -> Int
search trees x =
  let p = trees M.! x
   in if p == x then p else search trees p

processInput :: String -> ([Pos], Trees)
processInput s = (map parse ls, build (length ls))
 where
  ls = lines s
  parse ln =
    let xs = splitOn "," ln
     in (read (head xs), read (xs !! 1), read (last xs))
  build n = M.fromList [(i, i) | i <- [0 .. n - 1]]

buildDist :: [Pos] -> DistRecord
buildDist ps =
  M.fromListWith
    (++)
    [ (dist p1 p2, [(i, j)])
    | (i, p1) <- zip [0 ..] ps
    , (j, p2) <- zip [0 ..] ps
    , i < j
    ]
 where
  dist (x1, y1, z1) (x2, y2, z2) =
    sum $ map s [x1 - x2, y1 - y2, z1 - z2]
  s x = x * x

step :: (Trees, DistRecord) -> (Trees, DistRecord)
step (trees, rcd) = (join from to, rcd'')
 where
  ((dist, pairs), rcd') = M.deleteFindMin rcd
  ((from, to), ts) = (head pairs, tail pairs)
  rcd'' = if null ts then rcd' else M.insert dist ts rcd'
  join x y = M.insert (search trees x) (search trees y) trees

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n - 1) f (f x)

solution1 :: String -> IO ()
solution1 s =
  print
    . product
    . take 3
    . sortBy (flip compare)
    . count
    . findRoots
    $ fst (nTimes 1000 step (trees, rcd))
 where
  (pos, trees) = processInput s
  rcd = buildDist pos
  n = M.size trees
  findRoots t = map (search t) [1 .. n - 1]
  count = M.elems . M.fromListWith (+) . map (,1 :: Int)

run :: Int -> (Trees, DistRecord) -> (Int, Int)
run n (trees, rcd)
  | all (== head rts) (tail rts) =
      head . snd $ M.findMin rcd
  | otherwise = run n (trees', rcd')
 where
  (trees', rcd') = step (trees, rcd)
  rts = map (search trees') [1 .. n - 1]

solution2 :: String -> IO ()
solution2 s = print (x1 * x2)
 where
  (pos, trees) = processInput s
  (i, j) = run n (trees, buildDist pos)
  (x1, _, _) = pos !! i
  (x2, _, _) = pos !! j
  n = M.size trees
