module Year2025.Day08V2 (solution1, solution2) where

-- Try to use mutable vector
-- run this with:
-- stack run -- 2025 8 -1
-- stack run -- 2025 8 -2
-- This is just a test for using mutable vector
-- The performance improvement is very little
-- compared to Map version
-- So this version is not advised

import qualified Data.IntMap as M
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Vector.Mutable as V

type Trees = V.IOVector Int
type Pos = (Int, Int, Int)
type DistRecord = M.IntMap [(Int, Int)]

search :: Int -> Trees -> IO (Int, Trees)
search x trees = do
  g <- V.read trees x
  if g == x
    then return (g, trees)
    else do
      (pg, t) <- search g trees
      V.write t x pg
      return (pg, t)

join :: Int -> Int -> Trees -> IO Trees
join x y trees = do
  (g1, t) <- search x trees
  (g2, t') <- search y t
  V.write t' g2 g1
  return t'

processInput :: String -> IO ([Pos], Trees)
processInput s = do
  let ls = lines s
      parse ln =
        let xs = splitOn "," ln
         in (read (head xs), read (xs !! 1), read (last xs))
  trees <- V.generate (length ls) id
  return (map parse ls, trees)

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

step :: (Trees, DistRecord) -> IO (Trees, DistRecord)
step (trees, rcd) = do
  let ((dist, pairs), rcd') = M.deleteFindMin rcd
      ((from, to), ts) = (head pairs, tail pairs)
      rcd'' = if null ts then rcd' else M.insert dist ts rcd'
  trees' <- join from to trees
  return (trees', rcd'')

nTimes :: Int -> (a -> IO a) -> a -> IO a
nTimes 0 _ x = return x
nTimes n f x = f x >>= nTimes (n - 1) f

solution1 :: String -> IO ()
solution1 s = do
  putStrLn "Mutable version"
  (pos, trees) <- processInput s
  let rcd = buildDist pos
  (t, _) <- nTimes 1000 step (trees, rcd)
  let n = V.length t
  roots <- map fst <$> mapM (`search` t) [0 .. n - 1]
  let count = M.elems . M.fromListWith (+) $ map (,1 :: Int) roots
  print . product . take 3 $ sortBy (flip compare) count

run :: Int -> (Trees, DistRecord) -> IO (Int, Int)
run n (trees, rcd) = do
  (t, rcd') <- step (trees, rcd)
  rs <- mapM (`search` t) [1 .. n - 1]
  let t' = snd (last rs)
      rts = map fst rs
  if all (== head rts) (tail rts)
    then
      return . head . snd $ M.findMin rcd
    else run n (t', rcd')

solution2 :: String -> IO ()
solution2 s = do
  putStrLn "Mutable version"
  (pos, trees) <- processInput s
  let n = V.length trees
  (i, j) <- run n (trees, buildDist pos)
  let (x1, _, _) = pos !! i
      (x2, _, _) = pos !! j
  print $ x1 * x2
