module Year2023.Day01 (solution1, solution2) where

import Common.Utils (safeHead)
import Data.Char (isDigit)

processInput :: (String -> String) -> String -> [Int]
processInput f = map (extractNum . f) . lines
 where
  extractNum =
    read
      . (\s -> [safeHead s, last s])

solution1 :: String -> String
solution1 =
  show
    . sum
    . processInput (filter isDigit)

strToNum :: String -> String
strToNum "" = ""
strToNum ('o' : 'n' : 'e' : cs) = '1' : strToNum cs
strToNum ('t' : 'w' : 'o' : cs) = '2' : strToNum cs
strToNum ('t' : 'h' : 'r' : 'e' : 'e' : cs) =
  '3' : strToNum cs
strToNum ('f' : 'o' : 'u' : 'r' : cs) =
  '4' : strToNum cs
strToNum ('f' : 'i' : 'v' : 'e' : cs) =
  '5' : strToNum cs
strToNum ('s' : 'i' : 'x' : cs) = '6' : strToNum cs
strToNum ('s' : 'e' : 'v' : 'e' : 'n' : cs) =
  '7' : strToNum cs
strToNum ('e' : 'i' : 'g' : 'h' : 't' : cs) =
  '8' : strToNum cs
strToNum ('n' : 'i' : 'n' : 'e' : cs) =
  '9' : strToNum cs
strToNum (c : cs)
  | isDigit c = c : strToNum cs
  | otherwise = strToNum cs

solution2 :: String -> String
solution2 = show . sum . processInput strToNum
