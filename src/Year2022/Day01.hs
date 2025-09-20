module Year2022.Day01 (solution1, solution2) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))

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
  . fromDown
  . sum
  . take 3
  . sort
  . map (Down . sum)
  . parseInput
 where
  fromDown (Down x) = x