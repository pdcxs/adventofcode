module Year2023.Day11 (solution1, solution2) where

import qualified Data.Set as S

type Pos = (Int, Int)

processInput :: String -> (Int, Int, S.Set Pos)
processInput s =
 ( length (head ls)
 , length ls
 , parseMap 0 0 S.empty s
 )
 where
  ls = lines s

parseMap :: Int -> Int -> S.Set Pos -> String -> S.Set Pos
parseMap _ _ s [] = s
parseMap x y s ('#' : ss) =
 parseMap (x + 1) y (S.insert (x, y) s) ss
parseMap _ y s ('\n' : ss) =
 parseMap 0 (y + 1) s ss
parseMap x y s (_ : ss) =
 parseMap (x + 1) y s ss

getColums :: S.Set Pos -> S.Set Int
getColums = S.map fst

getRows :: S.Set Pos -> S.Set Int
getRows = S.map snd

updateColumn :: Int -> Int -> S.Set Int -> Pos -> Pos
updateColumn scale width columns (x, y) =
 (x + (scale - 1) * n, y)
 where
  emptyClms =
   [ c
   | c <- [0 .. width - 1]
   , c `S.notMember` columns
   , c < x
   ]
  n = length emptyClms

updateRow :: Int -> Int -> S.Set Int -> Pos -> Pos
updateRow scale height rows (x, y) =
 (x, y + (scale - 1) * n)
 where
  emptyRows =
   [ c
   | c <- [0 .. height - 1]
   , c `S.notMember` rows
   , c < y
   ]
  n = length emptyRows

getAnswer :: Int -> String -> String
getAnswer scale s =
 show $
  ( sum
     [ abs (x - x1) + abs (y - y1)
     | (x, y) <- ps''
     , (x1, y1) <- ps''
     , (x, y) /= (x1, y1)
     ]
  )
   `div` 2
 where
  (width, height, ps) = processInput s
  columns = getColums ps
  rows = getRows ps
  ps' = S.map (updateColumn scale width columns) ps
  ps'' = S.toList $ S.map (updateRow scale height rows) ps'

solution1 :: String -> String
solution1 = getAnswer 2

solution2 :: String -> String
solution2 = getAnswer 1000000
