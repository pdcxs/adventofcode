module Year2024.Day10 (solution1, solution2) where

import Data.Char (ord)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Vector as V
import Data.Vector ((!?))

type Loc = (Int, Int)

type Map = V.Vector (V.Vector Int)

processInput :: String -> ([Loc], Map)
processInput s = (starts, m)
  where
    ls = lines s
    m = parseMap ls
    w = V.length (V.head m)
    h = V.length m
    starts =
      [ (x, y)
        | x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          get m (x, y) == Just 0
      ]

parseMap :: [String] -> Map
parseMap = V.fromList . map (V.fromList . map toInt)
  where
    toInt c = ord c - ord '0'

get :: Map -> Loc -> Maybe Int
get m (x, y) = do
  row <- m !? y
  row !? x

getDest :: Map -> Loc -> [Loc]
getDest m loc =
  case get m loc of
    Nothing -> []
    Just 9 -> [loc]
    Just n ->
      concat
        [ getDest m next
          | next <- neighbors loc,
            get m next == Just (n + 1)
        ]

neighbors :: Loc -> [Loc]
neighbors (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

solution1 :: String -> String
solution1 s =
  show $
    sum $
      map (length . nubOrd . getDest m) starts
  where
    (starts, m) = processInput s

solution2 :: String -> String
solution2 s =
  show $
    sum $
      map (pathNum m) starts
  where
    (starts, m) = processInput s

pathNum :: Map -> Loc -> Int
pathNum m loc =
  case get m loc of
    Nothing -> 0
    Just 9 -> 1
    Just n ->
      sum
        [ pathNum m next
          | next <- neighbors loc,
            get m next == Just (n + 1)
        ]