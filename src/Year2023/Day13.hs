module Year2023.Day13 (solution1, solution2) where

import Data.Char (ord)
import Data.List (uncons)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)

processInput :: String -> [[String]]
processInput = splitWhen null . lines

transpose :: [String] -> [String]
transpose [] = []
transpose ss@([] : _) = ss
transpose ss =
 map fst ls
  : transpose (map snd ls)
 where
  ls = map (fromJust . uncons) ss

isMissMirror :: [String] -> Int -> Int -> Bool
isMissMirror ss dist n =
 sum (map abs loss) == dist
 where
  splits = map (splitAt n) ss
  lefts = map (map ord . reverse . fst) splits
  rights = map (map ord . snd) splits
  loss =
   concatMap
    (uncurry $ zipWith subtract)
    (zip lefts rights)

solve :: Int -> [String] -> Int
solve dist cs =
 sum xs + sum (map (* 100) ys)
 where
  cn = length (head cs)
  rs = transpose cs
  rn = length cs
  xs = filter (isMissMirror cs dist) [1 .. cn - 1]
  ys = filter (isMissMirror rs dist) [1 .. rn - 1]

solution1 :: String -> IO ()
solution1 = print . sum . map (solve 0) . processInput

solution2 :: String -> IO ()
solution2 =
 print
  . sum
  . map (solve $ ord '.' - ord '#')
  . processInput
