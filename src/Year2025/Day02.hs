module Year2025.Day02 (solution1, solution2) where

import Data.List.Split (splitOn)

type Range = (Int, Int)

processInput :: String -> [Range]
processInput = map (parse . splitOn "-") . splitOn ","
 where
  parse [x1, x2] = (read x1, read x2)
  parse xs = error ("Unknown " ++ show xs)

scan :: (Int -> Bool) -> Int -> Range -> Int
scan predictor result (start, end)
  | start > end = result
  | predictor start =
      scan predictor (result + start) (start + 1, end)
  | otherwise =
      scan predictor result (start + 1, end)

isBad :: Int -> Bool
isBad n = even len && left == right
 where
  s = show n
  len = length s
  (left, right) = splitAt (len `div` 2) s

solution1 :: String -> IO ()
solution1 = print . sum . map (scan isBad 0) . processInput

isBad' :: Int -> Bool
isBad' n = any isComposed ls
 where
  s = show n
  len = length s
  ls = [1 .. len `div` 2]
  isComposed i =
    let (d, r) = len `divMod` i
     in r == 0 && concat (replicate d (take i s)) == s

solution2 :: String -> IO ()
solution2 =
  print . sum . map (scan isBad' 0) . processInput
