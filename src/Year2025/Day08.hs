module Year2025.Day08 (solution1, solution2) where

import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.List.Split (splitOn)

type FindUnionSet = M.IntMap Int

build :: Int -> FindUnionSet
build n = M.fromList (map (\x -> (x, x)) [0 .. n - 1])

search :: Int -> FindUnionSet -> (Int, FindUnionSet)
search x fus
  | p == x || p == pp = (p, fus)
  | otherwise = (pp, M.insert x pp fus')
 where
  p = fus M.! x
  (pp, fus') = search p fus

join :: Int -> Int -> FindUnionSet -> FindUnionSet
join x y fus =
  let (r1, fus1) = search x fus
      (r2, fus2) = search y fus1
   in M.insert r2 r1 fus2

type Pos = (Int, Int, Int)
type DistRecord = M.IntMap [(Int, Int)]

processInput :: String -> ([Pos], FindUnionSet)
processInput s = (map parse ls, build (length ls))
 where
  ls = lines s
  parse ln =
    let xs = splitOn "," ln
     in (read (head xs), read (xs !! 1), read (last xs))

buildDist :: [Pos] -> DistRecord
buildDist ps =
  M.fromListWith
    (++)
    [ (dist p1 p2, [(i, j)])
    | (i, p1) <- idx
    , (j, p2) <- idx
    , i < j
    ]
 where
  idx = zip [0 ..] ps
  dist (x1, y1, z1) (x2, y2, z2) =
    (x1 - x2) * (x1 - x2)
      + (y1 - y2) * (y1 - y2)
      + (z1 - z2) * (z1 - z2)

step ::
  (FindUnionSet, DistRecord) ->
  (FindUnionSet, DistRecord)
step (fus, rcd) = (join from to fus, rcd'')
 where
  ((dist, pairs), rcd') = M.deleteFindMin rcd
  (from, to) = head pairs
  rcd'' =
    let ts = tail pairs
     in if null ts then rcd' else M.insert dist ts rcd'

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n - 1) f (f x)

findRoots ::
  Int -> Int -> FindUnionSet -> (FindUnionSet, [Int])
findRoots n idx fus
  | n == idx = (fus, [])
  | otherwise =
      let (g, fus') = search idx fus
          (fus'', rts) = findRoots n (idx + 1) fus'
       in (fus'', g : rts)

count :: [Int] -> [Int]
count xs = M.elems $ M.fromListWith (+) $ map (,1) xs

solution1 :: String -> IO ()
solution1 s =
  print
    . product
    . take 3
    . sortBy (flip compare)
    . count
    . snd
    . findRoots n 0
    $ fst (nTimes 1000 step (fus, rcd))
 where
  (pos, fus) = processInput s
  rcd = buildDist pos
  n = M.size fus

connect ::
  Int ->
  (FindUnionSet, DistRecord) ->
  (Int, Int)
connect n (fus, rcd)
  | all (== head rts) (tail rts) =
      head . snd $ M.findMin rcd
  | otherwise = connect n (fus'', rcd')
 where
  (fus', rcd') = step (fus, rcd)
  (fus'', rts) = findRoots n 0 fus'

solution2 :: String -> IO ()
solution2 s = print (x1 * x2)
 where
  (pos, fus) = processInput s
  (i, j) = connect n (fus, buildDist pos)
  (x1, _, _) = pos !! i
  (x2, _, _) = pos !! j
  n = M.size fus
