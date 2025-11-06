{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2024.Day13 (solution1, solution2) where

import Data.Char (isDigit, isSpace)
import Data.List.Split (splitWhen)

processInput :: String -> [[Integer]]
processInput s = map (concatMap extraceNumber) inputs
 where
  inputs = splitWhen null $ lines s
  extraceNumber =
    map read
      . words
      . filter (\c -> isDigit c || isSpace c)

isInteger :: Double -> Bool
isInteger x =
  x == fromIntegral (round x :: Integer)

-- Just solve an equation
solve :: [Integer] -> Integer
solve nums =
  if isInteger a && isInteger b
    then
      3 * round a + round b
    else 0
 where
  [ax, ay, bx, by, tx, ty] = map fromIntegral nums
  den = ax * by - ay * bx
  n1 = by * tx - bx * ty
  n2 = ax * ty - ay * tx
  a = n1 / den
  b = n2 / den

solution1 :: String -> IO ()
solution1 =
  print
    . sum
    . map solve
    . processInput

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . map (solve . fix)
    . processInput
 where
  fix x =
    take 4 x
      ++ map (+ 10000000000000) (drop 4 x)
