module Year2022.Day13 (solution1, solution2) where

import Control.Applicative (Alternative (some))
import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (sort)
import Data.List.Split (splitWhen)
import Text.Parsec (
  char,
  digit,
  parse,
  sepBy,
  (<|>),
 )
import Text.Parsec.String (Parser)

data Input = N Int | L [Input] deriving (Eq, Show)

instance Ord Input where
  compare (N x) (N y) = compare x y
  compare (N x) (L xs) = compare (L [N x]) (L xs)
  compare (L xs) (N x) = compare (L xs) (L [N x])
  compare (L []) (L []) = EQ
  compare (L []) (L _) = LT
  compare (L _) (L []) = GT
  compare (L (x : xs)) (L (y : ys))
    | x == y = compare (L xs) (L ys)
    | otherwise = compare x y

inputParser :: Parser Input
inputParser = N . read <$> some digit <|> listParser

listParser :: Parser Input
listParser = do
  void $ char '['
  inputs <- sepBy inputParser (char ',')
  void $ char ']'
  return $ L inputs

parseLine :: String -> Input
parseLine = fromRight undefined . parse inputParser ""

processInput :: String -> [[Input]]
processInput = map (map parseLine) . splitWhen null . lines

solution1 :: String -> IO ()
solution1 =
  print
    . sum
    . map fst
    . filter snd
    . zip [(1 :: Int) ..]
    . map (\xs -> head xs < xs !! 1)
    . processInput

solution2 :: String -> IO ()
solution2 =
  print
    . product
    . getIndex 1
    . sort
    . ([get 2, get 6] ++)
    . concat
    . processInput
 where
  get n = L [L [N n]]
  getIndex :: Int -> [Input] -> [Int]
  getIndex i (x : xs)
    | x == get 2 = i : getIndex (i + 1) xs
    | x == get 6 = [i]
    | otherwise = getIndex (i + 1) xs
  getIndex _ _ = undefined
