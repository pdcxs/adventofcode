{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2022.Day04 (solution1, solution2) where

import Data.List.Split (splitOn)

type Range = (Int, Int)

processInput :: String -> [(Range, Range)]
processInput = map parseLine . lines
 where
  parseLine ln =
   let [r1, r2] = splitOn "," ln
    in (parseRange r1, parseRange r2)
  parseRange r =
   let [a, b] = splitOn "-" r
    in (read a, read b)

isIn :: Range -> Range -> Bool
isIn (a, b) (c, d) =
 a >= c && b <= d
  || c >= a && d <= b

solution1 :: String -> IO ()
solution1 =
 print
  . length
  . filter (uncurry isIn)
  . processInput

isOverlap :: Range -> Range -> Bool
isOverlap (a, b) (c, d) =
 not (b < c || a > d)

solution2 :: String -> IO ()
solution2 = 
 print
  . length
  . filter (uncurry isOverlap)
  . processInput