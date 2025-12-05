module Year2025.Day05 (solution1, solution2) where

import Data.List.Split (splitOn, splitWhen)

type Range = (Int, Int)

processInput :: String -> ([Range], [Int])
processInput s = (map parseRange rgs, map read xs)
 where
  ls = lines s
  parts = splitWhen null ls
  rgs = head parts
  xs = last parts
  parseRange ln =
    let ns = splitOn "-" ln
     in (read (head ns), read (last ns))

isIn :: Int -> [Range] -> Bool
isIn _ [] = False
isIn x ((s, e) : rgs)
  | x >= s && x <= e = True
  | otherwise = isIn x rgs

count :: ([Range], [Int]) -> Int
count (rgs, xs) = length $ filter (`isIn` rgs) xs

solution1 :: String -> IO ()
solution1 = print . count . processInput

removeRng :: Range -> Range -> [Range]
removeRng (x, y) (s, e)
  | x < s && y > e = [(x, s - 1), (e + 1, y)]
  | x < s && y >= s && y <= e = [(x, s - 1)]
  | x < s && y < s = [(x, y)]
  | x <= e && y <= e = []
  | x <= e && y > e = [(e + 1, y)]
  | otherwise = [(x, y)]

mergeRanges :: [Range] -> [Range]
mergeRanges = go []
 where
  go rs [] = rs
  go rs (x : xs) = go rs' xs
   where
    rs' = x' ++ rs
    x' = removeRngs [x] rs
  removeRngs x [] = x
  removeRngs [] _ = []
  removeRngs x (r : rs) =
    removeRngs (concatMap (`removeRng` r) x) rs

countRngs :: [Range] -> Int
countRngs [] = 0
countRngs ((s, e) : rs) = e - s + 1 + countRngs rs

solution2 :: String -> IO ()
solution2 =
  print
    . countRngs
    . mergeRanges
    . fst
    . processInput
