module Year2025.Day12 (solution1, solution2) where

import Data.List.Split (splitWhen)

type Shape = [String]
type Area = (Int, Int, [Int])

processInput :: String -> ([Shape], [Area])
processInput s =
  (map getShape (init blocks), map getArea (last blocks))
 where
  blocks = splitWhen null (lines s)
  getShape = tail
  getArea ln =
    let ws = words ln
        h = head ws
        x = read $ take 2 h
        y = read $ take 2 $ drop 3 h
     in (x, y, map read (tail ws))

-- Don't know why, but this works
check :: [Shape] -> Area -> Bool
check shapes (x, y, cnts) = x * y >= sum nums
 where
  nums =
    [ n * c
    | (s, n) <- zip shapes cnts
    , let c = length (concatMap (filter (== '#')) s)
    ]

solution1 :: String -> IO ()
solution1 s = print $ length $ filter (check shapes) areas
 where
  (shapes, areas) = processInput s

solution2 :: String -> IO ()
solution2 _ = print "We are finisehd!"
