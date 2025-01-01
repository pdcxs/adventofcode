module Year2024.Day5 (solution1, solution2) where

import Algebra.Graph.AdjacencyIntMap
import Algebra.Graph.AdjacencyIntMap.Algorithm
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void, guard)
import Data.Either (fromRight)

ruleParser :: Parser (Int, Int)
ruleParser = do
  v1 <- read <$> many digit
  void $ char '|'
  v2 <- read <$> many digit
  return (v1, v2)

updateParser :: Parser [Int]
updateParser = sepBy (read <$> many digit) (char ',')

parser :: Parser ([(Int, Int)], [[Int]])
parser = do
  rules <- many (ruleParser <* endOfLine)
  void endOfLine
  updates <- sepBy updateParser endOfLine
  return (rules, updates)

processInput :: String -> ([(Int, Int)], [[Int]])
processInput = fromRight undefined . parse parser ""

solution1 :: String -> String
solution1 s = show $ sum $ map middle good
  where
    (rules, updates) = processInput s
    g = edges rules
    good = [p| p <- updates, isTopSortOf p (induce (`elem` p) g)]

middle :: [Int] -> Int
middle xs = xs !! m
  where
    m = length xs `div` 2

solution2 :: String -> String
solution2 s = show $ sum $ map middle sorted
  where
    (rules, updates) = processInput s
    g = edges rules
    bad = do
      p <- updates
      let subg = induce (`elem` p) g
      guard (not (isTopSortOf p subg))
      return subg
    sorted = map (fromRight undefined . topSort) bad