module Year2025.Day05 (solution1, solution2) where

import Data.List.Split (splitOn, splitWhen)

type Range = (Int, Int)

processInput :: String -> ([Range], [Int])
processInput s = (map getRange rgs, map read xs)
 where
  pts = splitWhen null $ lines s
  rgs = head pts
  xs = last pts
  getRange ln =
    let ns = splitOn "-" ln
     in (read (head ns), read (last ns))

inRngs :: Int -> [Range] -> Bool
inRngs x = any (\(s, e) -> x >= s && x <= e)

count :: [Range] -> [Int] -> Int
count rgs = length . filter (`inRngs` rgs)

solution1 :: String -> IO ()
solution1 = print . uncurry count . processInput

diffRng :: Range -> Range -> [Range]
diffRng (x, y) (s, e)
  | x < s && y < s = [(x, y)]
  | x < s && y <= e = [(x, s - 1)]
  | x < s && y > e = [(x, s - 1), (e + 1, y)]
  | x <= e && y <= e = []
  | x <= e && y > e = [(e + 1, y)]
  | otherwise = [(x, y)]

addTo :: Range -> [Range] -> [Range]
addTo r rs = delete [r] rs ++ rs
 where
  delete inputs [] = inputs
  delete [] _ = []
  delete inputs (x : xs) =
    delete (concatMap (`diffRng` x) inputs) xs

deleteOverlaps :: [Range] -> [Range]
deleteOverlaps = foldr addTo []

countRange :: Range -> Int
countRange (s, e) = e - s + 1

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . map countRange
    . deleteOverlaps
    . fst
    . processInput
