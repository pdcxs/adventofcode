module Year2024.Day11 (solution1, solution2) where

import qualified Data.IntMap.Strict as M
import Data.MemoTrie (memo2)

processInput :: String -> [Int]
processInput = map read . words

blink :: Int -> [Int]
blink 0 = [1]
blink n
 | even len =
   [read left, read right]
 | otherwise = [2024 * n]
 where
  s = show n
  len = length s
  half = len `div` 2
  left = take half s
  right = drop half s

-- Two methods here
-- First one: use memoize
-- Second one: use Map to record stones number

countStone :: Int -> Int -> Int
countStone = memo2 f
 where
  f 0 _ = 1
  f t n = sum $ map (countStone (t - 1)) $ blink n

step :: M.IntMap Int -> M.IntMap Int
step m =
 M.fromListWith
  (+)
  [ (s', n)
  | (s, n) <- M.assocs m
  , s' <- blink s
  ]

countStone' :: Int -> Int -> Int
countStone' times stone =
 sum (iterate step (M.singleton stone 1) !! times)

countStones :: (Int -> Int -> Int) -> Int -> String -> String
countStones f times s =
 show . sum $
  map (f times) (processInput s)

solution1 :: String -> String
solution1 = countStones countStone 25

-- You can also use countStones countStone 75.
-- Both methods are OK.
solution2 :: String -> String
solution2 = countStones countStone' 75
