module Year2024.Day5 (solution1, solution2) where

import Control.Monad (void)
import Data.Either (fromRight)
import Text.Parsec
import Text.Parsec.String

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
  updates <- many (endOfLine *> updateParser)
  return (rules, updates)

processInput :: String -> ([(Int, Int)], [[Int]])
processInput = fromRight undefined . parse parser ""

solution1 :: String -> String
solution1 s = show $ sum $ map middle good
  where
    (rules, updates) = processInput s
    good = filter (isTopSortOf rules) updates

isTopSortOf :: [(Int, Int)] -> [Int] -> Bool
isTopSortOf _ [] = True
isTopSortOf g (x : xs) =
  not (or [(y, x) `elem` g | y <- xs])
    && isTopSortOf g xs

middle :: [Int] -> Int
middle xs = xs !! m
  where
    m = length xs `div` 2

solution2 :: String -> String
solution2 s = show $ sum $ map middle sorted
  where
    (rules, updates) = processInput s
    bad = filter (not . isTopSortOf rules) updates
    sorted = map (topSort rules) bad

topSort :: [(Int, Int)] -> [Int] -> [Int]
topSort _ [] = []
topSort _ [x] = [x]
topSort g (x : xs) = insert x (topSort g xs)
  where
    insert y [] = [y]
    insert y (z : zs) =
      if (y, z) `elem` g
        then
          y : z : zs
        else
          z : insert y zs