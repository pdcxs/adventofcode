module Year2022.Day01 (solution1, solution2) where

import Data.List (sortBy)
import Data.List.Split (splitOn)

parseInput :: String -> [[Int]]
parseInput = map (map read) . splitOn [""] . lines

solution1 :: String -> IO ()
solution1 =
  print
    . maximum
    . map sum
    . parseInput

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . take 3
    . sortBy (flip compare)
    . map sum
    . parseInput
