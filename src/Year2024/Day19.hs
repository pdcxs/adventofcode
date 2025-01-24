module Year2024.Day19 (solution1, solution2) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)
import Text.Regex.TDFA

processInput :: String -> (String, [String])
processInput s = (regex, inputs)
 where
  ls = lines s
  regex = "\\`(" ++ go (head ls) ++ ")*\\'"
  go [] = []
  go (',' : xs) = '|' : go xs
  go (' ' : xs) = go xs
  go (x : xs) = x : go xs
  inputs = drop 2 ls

solution1 :: String -> String
solution1 s =
  show . length $
    filter (\i -> i =~ regex :: Bool) inputs
 where
  (regex, inputs) = processInput s

processInput' :: String -> (String, [String], [String])
processInput' s = (regex, patterns, drop 2 ls)
 where
  ls = lines s
  patterns = splitOn ", " (head ls)
  regex = "\\`(" ++ intercalate "|" patterns ++ ")*\\'"

-- Use Trie Tree should be faster
-- But I think now it's simple and clear
-- with tolerated performance
count :: [String] -> String -> Int
count _ [] = 1
count patterns s =
  sum
    ( map
        (memo2 count patterns)
        remains
    )
 where
  remains =
    [ drop (length p) s
    | p <- patterns
    , take (length p) s == p
    ]

solution2 :: String -> String
solution2 s = show . sum $ map (count patterns) targets'
 where
  targets' = filter (\i -> i =~ regex :: Bool) targets
  (regex, patterns, targets) = processInput' s
