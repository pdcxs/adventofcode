module Year2023.Day04 (solution1, solution2) where

import Data.List.Split
import qualified Data.Map.Strict as M

processInput :: String -> [([Int], [Int])]
processInput = map processLine . lines
 where
  processLine = extractNumbers . last . splitOn ":"
  extractNumbers s =
   let nss = splitOn "|" s
       ns = map (map read . words) nss
    in (head ns, last ns)

getPoint :: ([Int], [Int]) -> Int
getPoint (xs, ys)
 | n == 0 = 0
 | otherwise = 2 ^ (n - 1)
 where
  n = length [p | p <- ys, p `elem` xs]

solution1 :: String -> String
solution1 = show . sum . map getPoint . processInput

getInstance :: Int -> M.Map Int Int -> [([Int], [Int])] -> Int
getInstance _ m [] = sum (M.elems m)
getInstance i m (x : xs)
 | next == 0 = getInstance (i + 1) m xs
 | otherwise = getInstance (i + 1) m' xs
 where
  count (as, bs) = length [p | p <- bs, p `elem` as]
  next = length (take (count x) xs)
  cur = m M.! i
  cards = [(c, cur) | c <- [i + 1 .. i + next]]
  m' = M.unionWith (+) m (M.fromList cards)

solution2 :: String -> String
solution2 s = show (getInstance 1 m rs)
 where
  rs = processInput s
  len = length rs
  m = M.fromList [(i, 1) | i <- [1 .. len]]
