module Year2024.Day01 (solution1, solution2) where

import Data.List (sort)

processInput :: String -> ([Int], [Int])
processInput s = (map head nums, map last nums)
 where
  ls = lines s
  nums = map (map read . words) ls

solution1 :: String -> String
solution1 s = show $ sum $ map abs sl
 where
  (lst1, lst2) = processInput s
  sl = zipWith subtract (sort lst1) (sort lst2)

solution2 :: String -> String
solution2 s =
 show $
  sum $
   zipWith (*) lst1 $
    map (count lst2) lst1
 where
  (lst1, lst2) = processInput s

count :: [Int] -> Int -> Int
count xs target = length $ filter (== target) xs
