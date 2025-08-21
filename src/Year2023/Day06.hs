module Year2023.Day06 (solution1, solution2) where

import Data.Char (isDigit)

processInput :: String -> [(Double, Double)]
processInput s = rs
 where
  ls = lines s
  nums = map (map read . tail . words) ls
  rs = zip (head nums) (last nums)

-- solve equation:
-- x * (t - x) == d
-- x^2 - t * x + d == 0
solve :: (Double, Double) -> Int
solve (t, d) = floor x1 - ceiling x2 + 1 - fix1 - fix2
 where
  delta = sqrt $ t * t - 4 * d
  x1 = (t + delta) / 2
  x2 = (t - delta) / 2
  fix1 = if isRound x1 then 1 else 0
  fix2 = if isRound x2 then 1 else 0

isRound :: Double -> Bool
isRound x = abs (r - x) < 1e-5
 where
  r = fromIntegral (round x :: Int)

solution1 :: String -> IO ()
solution1 =
 print
  . product
  . map solve
  . processInput

processInput' :: String -> (Double, Double)
processInput' s =
 (getNum (head ls), getNum (last ls))
 where
  ls = lines s
  getNum = read . filter isDigit

solution2 :: String -> IO ()
solution2 = print . solve . processInput'
