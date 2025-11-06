module Year2024.Day25 (solution1, solution2) where

import Data.List (partition)
import Data.List.Split (splitWhen)

processInput ::
  String -> ([[Int]], [[Int]])
processInput s =
  ( map getHeight locks
  , map getHeight keys
  )
 where
  ls = lines s
  (locks, keys) =
    partition ((== '#') . head . head) $
      splitWhen null ls

getHeight :: [String] -> [Int]
getHeight ls = map count cols
 where
  cols =
    map (\idx -> map (!! idx) ls) [0 .. 4]
  count c =
    if head c == '.'
      then length (dropWhile (== '.') c) - 1
      else length (takeWhile (== '#') c) - 1

fit :: [Int] -> [Int] -> Bool
fit xs = all (<= 5) . zipWith (+) xs

solution1 :: String -> IO ()
solution1 s =
  print . length $
    [ (l, k) | l <- locks, k <- keys, fit l k
    ]
 where
  (locks, keys) = processInput s

solution2 :: String -> IO ()
solution2 = putStrLn . const "We make it!"
