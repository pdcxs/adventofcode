module Year2023.Day14 (solution1, solution2) where

import Data.List (elemIndex, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as S

type Coord = Int
type Pos = (Coord, Coord)
type Dir = (Coord, Coord)
type Record = M.Map Coord (S.Set Coord)

processInput ::
  String ->
  ( Int -- width
  , Int -- height
  , [Pos] -- initial dynamic positions
  , Record -- row static positions
  , Record -- column static positions
  )
processInput s =
  (width, height, ps, rows, cols)
 where
  ls = lines s
  width = length (head ls)
  height = length ls
  (ps, rows, cols) =
    parseMap 0 0 [] M.empty M.empty s

parseMap ::
  Coord ->
  Coord ->
  [Pos] ->
  Record ->
  Record ->
  String ->
  ([Pos], Record, Record)
parseMap _ _ ps rs cs [] = (ps, rs, cs)
parseMap _ y ps rs cs ('\n' : ss) =
  parseMap 0 (y + 1) ps rs cs ss
parseMap x y ps rs cs ('O' : ss) =
  parseMap (x + 1) y ((x, y) : ps) rs cs ss
parseMap x y ps rs cs ('#' : ss) =
  parseMap
    (x + 1)
    y
    ps
    (M.insertWith S.union y (S.singleton x) rs)
    (M.insertWith S.union x (S.singleton y) cs)
    ss
parseMap x y ps rs cs (_ : ss) =
  parseMap (x + 1) y ps rs cs ss

tilt :: Int -> Int -> Record -> Record -> Dir -> [Pos] -> [Pos]
tilt _ _ _ _ _ [] = []
tilt width height rows cols dir ((x, y) : ps) =
  p' : remain
 where
  remain = tilt width height rows' cols' dir ps
  key = if fst dir == 0 then x else y
  val = if fst dir == 0 then y else x
  rcd = if fst dir == 0 then cols else rows
  bound =
    if fst dir < 0 || snd dir < 0
      then 0
      else
        if fst dir > 0 then width - 1 else height - 1
  (rcd', p) = update dir rcd bound key val
  p' = if fst dir == 0 then (x, p) else (p, y)
  cols' = if fst dir == 0 then rcd' else cols
  rows' = if snd dir == 0 then rcd' else rows

update ::
  Dir ->
  Record ->
  Int ->
  Coord ->
  Coord ->
  (Record, Coord)
update dir rcd bnd key val =
  ( M.insertWith
      S.union
      key
      (S.singleton newPos)
      rcd
  , newPos
  )
 where
  newPos = case rcd M.!? key of
    Nothing -> bnd
    Just ps -> case findFn val ps of
      Nothing -> bnd
      Just p -> p - sign
  sign = fst dir + snd dir
  findFn =
    if sign < 0
      then
        S.lookupLT
      else S.lookupGT

getAnswer :: Int -> [Pos] -> Int
getAnswer height =
  sum
    . map
      ((height -) . snd)

solution1 :: String -> String
solution1 s = show (getAnswer height ps')
 where
  (width, height, ps, rows, cols) = processInput s
  sps = sortBy (comparing snd) ps
  ps' = tilt width height rows cols (0, -1) sps

solution2 :: String -> String
solution2 s = show $ getAnswer height $ ps' !! idx
 where
  (width, height, ps, rows, cols) = processInput s
  sps = sortBy (comparing snd) ps
  ps' = cycleTilt width height rows cols sps
  cycles = getLoop ps'
  n = getLoopSize cycles
  preLen = fromJust $ elemIndex (head cycles) ps'
  idx = preLen + ((1000000000 - preLen - 1) `mod` n)

cycleTilt ::
  Int ->
  Int ->
  Record ->
  Record ->
  [Pos] ->
  [[Pos]]
cycleTilt width height rows cols ps = ps' : cont
 where
  ps1 = tilt width height rows cols (0, -1) ps
  sps1 = sortBy (comparing fst) ps1
  ps2 = tilt width height rows cols (-1, 0) sps1
  sps2 = sortBy (flip $ comparing snd) ps2
  ps3 = tilt width height rows cols (0, 1) sps2
  sps3 = sortBy (flip $ comparing fst) ps3
  ps4 = tilt width height rows cols (1, 0) sps3
  ps' = sortBy (comparing snd) ps4
  cont = cycleTilt width height rows cols ps'

getLoop :: [[Pos]] -> [[Pos]]
getLoop xs = go xs xs
 where
  go (a : as) (_ : b : bs) =
    if a == b then (a : as) else go as bs
  go _ _ = undefined

getLoopSize :: (Eq a) => [a] -> Int
getLoopSize [] = 0
getLoopSize (x : xs) = go xs 1
 where
  go (a : as) n =
    if a == x
      then n
      else go as (n + 1)
  go _ _ = undefined
