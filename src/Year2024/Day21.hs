module Year2024.Day21 (solution1, solution2) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.MemoTrie (memo3)

type Pos = (Int, Int)

type Keyboard = M.Map Char Pos

buildKeyboard :: [String] -> Keyboard
buildKeyboard = go 0 0 M.empty
 where
  go _ _ keyboard [] = keyboard
  go _ y keyboard ([] : ss) = go 0 (y + 1) keyboard ss
  go x y keyboard ((c : cs) : ss) =
    go
      (x + 1)
      y
      (M.insert c (x, y) keyboard)
      (cs : ss)

numKeyboard :: Keyboard
numKeyboard = buildKeyboard ["789", "456", "123", " 0A"]

dirKeyboard :: Keyboard
dirKeyboard = buildKeyboard [" ^A", "<v>"]

-- getPaths numKeyboard '0' '9'
-- [">^^^", "^^^>"]
getPaths :: Keyboard -> Char -> Char -> [String]
getPaths keyboard from to =
  [path ++ "A" | invalid /= (x2, y1)]
    ++ [ reverse path ++ "A"
       | x1 /= x2
       , y1 /= y2
       , invalid /= (x1, y2)
       ]
 where
  (x1, y1) = keyboard M.! from
  (x2, y2) = keyboard M.! to
  path =
    replicate (x2 - x1) '>'
      ++ replicate (x1 - x2) '<'
      ++ replicate (y2 - y1) 'v'
      ++ replicate (y1 - y2) '^'
  invalid = keyboard M.! ' '

shortestPath :: Int -> Char -> Char -> Int
shortestPath n f t =
  minimum $
    map
      (cntPath (n - 1))
      nextPaths
 where
  nextPaths = getPaths dirKeyboard f t

cntPath :: Int -> String -> Int
cntPath 0 s = length s
cntPath n p =
  sum $
    zipWith
      (memo3 shortestPath n)
      ('A' : p)
      p

solution :: Int -> String -> String
solution n s =
  show . sum $
    zipWith (*) nums cnts
 where
  ls = lines s
  nums = map (read . filter isDigit) ls
  paths =
    map
      ( \l ->
          zipWith
            (getPaths numKeyboard)
            ('A' : l)
            l
      )
      ls
  cnts =
    [ sum
      (map (minimum . map (cntPath n)) ps)
    | ps <- paths
    ]

solution1 :: String -> String
solution1 = solution 2

solution2 :: String -> String
solution2 = solution 25
