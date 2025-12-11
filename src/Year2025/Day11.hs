module Year2025.Day11 (solution1, solution2) where

import qualified Data.Map.Strict as M
import Data.MemoTrie

type Map = M.Map String [String]

processInput :: String -> Map
processInput = go M.empty . lines
 where
  go m [] = m
  go m (l : ls) = let (s, ns) = parseLine l in go (M.insert s ns m) ls
  parseLine l =
    let xs = words l
     in (init (head xs), tail xs)

search ::
  Map -> String -> String -> Int
search m from to = go from
 where
  go pos =
    if pos == to
      then 1
      else case m M.!? pos of
        Nothing -> 0
        Just ps -> sum $ map mg ps
  mg = memo go

solution1 :: String -> IO ()
solution1 = print . (\m -> search m "you" "out") . processInput

solution2 :: String -> IO ()
solution2 s = print $ s1 * s2 * s3 + s4 * s5 * s6
 where
  m = processInput s
  s1 = search m "svr" "fft"
  s2 = search m "fft" "dac"
  s3 = search m "dac" "out"
  s4 = search m "svr" "dac"
  s5 = search m "dac" "fft"
  s6 = search m "fft" "out"
