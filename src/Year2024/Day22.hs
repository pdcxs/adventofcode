module Year2024.Day22 (solution1, solution2) where

import Data.Bits (xor)
import Data.List (foldl1', tails)
import qualified Data.Map.Strict as M

processInput :: String -> [Int]
processInput = map read . lines

prune :: Int -> Int
prune = flip rem 16777216

next :: Int -> Int
next x = r
 where
  r1 = prune $ x `xor` (x * 64)
  r2 = prune $ r1 `xor` (r1 `quot` 32)
  r = prune $ r2 `xor` (r2 * 2048)

ntimes :: Int -> Int -> Int
ntimes n = (!! n) . iterate next

solution1 :: String -> IO ()
solution1 =
 print
  . sum
  . map (ntimes 2000)
  . processInput

getMap :: Int -> M.Map [Int] Int
getMap n = go M.empty rs
 where
  ns = map (`mod` 10) $ take 2001 $ iterate next n
  hikes = zipWith subtract (tail ns) ns
  rs = zip (drop 4 ns) (map (take 4) (tails hikes))
  go m [] = m
  go m ((x, hs) : nhs)
   | hs `M.member` m = go m nhs
   | otherwise = go (M.insert hs x m) nhs

solution2 :: String -> IO ()
solution2 =
 print
  . maximum
  . foldl1' (M.unionWith (+))
  . map getMap
  . processInput
