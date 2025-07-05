module Year2024.Day18 (
 solution1,
 solution2,
) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Pos = (Int, Int)

type LabledPos = (Int, Pos)

-- record: pos and its distance
type Record = M.Map Pos Int

size :: Int
-- size = 6 -- for test
size = 70 -- puzzle input

-- first parameter is counted positions
processInput :: Int -> String -> S.Set Pos
processInput n s = foldr S.insert S.empty locs
 where
  ls = take n $ lines s
  locs = map (getPos . map read . splitOn ",") ls
  getPos xs = (head xs, last xs)

end :: Pos
end = (size, size)

search :: S.Set Pos -> Record -> [LabledPos] -> Maybe Record
search walls record candidates
 | null candidates = Nothing
 | pos == end = Just record'
 | otherwise = search walls record' candidates'
 where
  ((score, pos), others) = getMin candidates
  record' = M.insertWith min pos score record
  cs' =
   [ (score + 1, p)
   | p <- neighbors pos
   , p `S.notMember` walls
   , p `M.notMember` record
   ]
  candidates' = combine cs' others

neighbors :: Pos -> [Pos]
neighbors (x, y) =
 filter
  valid
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)
  ]
 where
  valid (px, py) =
   px >= 0
    && px <= size
    && py >= 0
    && py <= size

getMin :: (Ord a) => [a] -> (a, [a])
getMin [] = undefined
getMin [x] = (x, [])
getMin (x : xs) =
 let (m, others) = getMin xs
  in if x < m
      then (x, m : others)
      else (m, x : others)

combine :: [LabledPos] -> [LabledPos] -> [LabledPos]
combine [] ys = ys
combine (x : xs) ys =
 combine xs (insert x ys)
 where
  insert a [] = [a]
  insert a@(sc, pos) (b@(sc', pos') : bs) =
   if pos == pos'
    then
     if sc < sc'
      then a : bs
      else b : bs
    else b : insert a bs

solution1 :: String -> String
solution1 s = show $ record M.! end
 where
  walls = processInput 1024 s
  record =
   fromJust $
    search
     walls
     M.empty
     [(0, (0, 0))]

processInput' :: String -> [Pos]
processInput' =
 map
  ( (\x -> (head x, last x))
     . map read
     . splitOn ","
  )
  . lines

-- search in reverse order
solution2 :: String -> String
solution2 s =
 init . tail . show $
  find pos walls
 where
  pos = reverse $ processInput' s
  walls = S.fromList (tail pos)

find :: [Pos] -> S.Set Pos -> Pos
find [] _ = undefined
find (p : ps) walls =
 let wall' = S.delete p walls
  in case search wall' M.empty [(0, (0, 0))] of
      Just _ -> p
      Nothing -> find ps wall'
