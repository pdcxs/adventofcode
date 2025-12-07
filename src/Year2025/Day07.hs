module Year2025.Day07 (solution1, solution2) where

import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)

type Map = M.IntMap S.IntSet -- key is y, value is [Pos]
type Record = M.IntMap Int -- key is Pos, value is count

processInput :: String -> (Map, Record)
processInput s = (m, M.singleton pos 1)
 where
  pos = fromJust $ elemIndex 'S' (head (lines s))
  m = M.fromList . zip [1 ..] $ getIdx (tail (lines s))
  getIdx = map (S.fromList . elemIndices '^')

step :: Int -> Map -> Record -> Maybe (Int, Record)
step y m rcd = do
  sps <- m M.!? y
  let (inter, unchanged) = M.partitionWithKey inSps rcd
      inSps k _ = k `S.member` sps
      left = M.mapKeysMonotonic pred inter
      right = M.mapKeysMonotonic succ inter
      rcd' = foldr1 (M.unionWith (+)) [unchanged, left, right]
  return (M.size inter, rcd')

count :: Int -> Int -> (Map, Record) -> (Int, Int)
count c y (m, rcd) = case step y m rcd of
  Nothing -> (c, sum rcd)
  Just (sps, rcd') -> count (c + sps) (y + 1) (m, rcd')

solution1 :: String -> IO ()
solution1 = print . fst . count 0 1 . processInput

solution2 :: String -> IO ()
solution2 = print . snd . count 0 1 . processInput
