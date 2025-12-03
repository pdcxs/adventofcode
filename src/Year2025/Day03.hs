module Year2025.Day03 (solution1, solution2) where

import Data.Char (ord)

processInput :: String -> [[Int]]
processInput = map (map (\c -> ord c - ord '0')) . lines

maxBut :: Int -> [Int] -> Int
maxBut n = maximum . reverse . drop n . reverse

behind :: Int -> [Int] -> [Int]
behind n (x : xs)
  | x == n = xs
  | otherwise = behind n xs
behind _ [] = error "behind error"

getMax :: Int -> [Int] -> Int
getMax n xs
  | n == 1 = maximum xs
  | otherwise =
      m * (10 ^ (n - 1))
        + getMax (n - 1) (behind m xs)
 where
  m = maxBut (n - 1) xs

solution1 :: String -> IO ()
solution1 = print . sum . map (getMax 2) . processInput

solution2 :: String -> IO ()
solution2 = print . sum . map (getMax 12) . processInput
