module Year2023.Day01 (solution1, solution2) where

import Data.Char (intToDigit, isDigit, ord)
import Data.Either (fromRight)
import Data.Functor (($>))
import Text.Parsec (
  anyChar,
  many,
  parse,
  string,
  try,
  (<|>),
 )
import Text.Parsec.String (Parser)

processInput :: String -> [Int]
processInput = map extractNum . lines
 where
  extractNum =
    read
      . (\s -> [head s, last s])
      . filter isDigit

solution1 :: String -> String
solution1 = show . sum . processInput

getNum :: String -> Int -> Parser Char
getNum s n = try (string s) $> c
 where
  c = intToDigit n

numStrs :: [String]
numStrs =
  [ "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

nums :: Parser Char
nums =
  foldr1 (<|>) $
    zipWith
      getNum
      numStrs
      [1 .. 9]

rnums :: Parser Char
rnums =
  foldr1 (<|>) $
    zipWith
      getNum
      (map reverse numStrs)
      [1 .. 9]

parseLine :: Parser Char -> Parser String
parseLine ns = many $ ns <|> anyChar

processInput' :: String -> [Int]
processInput' = map processLine . lines
 where
  processLine l =
    let
      l1 =
        fromRight "" $
          parse
            (parseLine nums)
            ""
            l
      l2 =
        fromRight "" $
          parse
            (parseLine rnums)
            ""
            (reverse l)
     in
      10 * firstNum l1 + firstNum l2
  firstNum = toInt . head . filter isDigit
  toInt c = ord c - ord '0'

solution2 :: String -> String
solution2 = show . sum . processInput'
