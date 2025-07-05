module Year2023.Day16 (solution1, solution2) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = Int
type Pos = (Coord, Coord)
type Dir = (Int, Int)
type State = (Pos, Dir)
type Map = M.Map Pos Char

processInput :: String -> (Int, Int, Map)
processInput s = (width, height, m)
 where
  m = parseMap 0 0 M.empty s
  ls = lines s
  width = length (head ls)
  height = length ls

parseMap :: Coord -> Coord -> Map -> String -> Map
parseMap _ _ m [] = m
parseMap _ y m ('\n' : ss) = parseMap 0 (y + 1) m ss
parseMap x y m (c : ss) =
 parseMap (x + 1) y (M.insert (x, y) c m) ss

step :: Int -> Int -> Map -> State -> [State]
step width height m (pos@(x, y), (dx, dy)) =
 case m M.!? pos of
  Just '-' -> if dy == 0 then forward else left ++ right
  Just '|' -> if dx == 0 then forward else up ++ down
  Just '/' ->
   if dx == 0
    then
     if dy < 0 then right else left
    else
     if dx < 0
      then down
      else up
  Just '\\' ->
   if dx == 0
    then
     if dy < 0 then left else right
    else
     if dx < 0
      then up
      else down
  Just _ -> forward
  _ -> []
 where
  forward =
   let p = (x + dx, y + dy)
    in [(p, (dx, dy)) | valid p]
  up =
   let p = (x, y - 1)
    in [(p, (0, -1)) | valid p]
  down =
   let p = (x, y + 1)
    in [(p, (0, 1)) | valid p]
  left =
   let p = (x - 1, y)
    in [(p, (-1, 0)) | valid p]
  right =
   let p = (x + 1, y)
    in [(p, (1, 0)) | valid p]
  valid (px, py) =
   px >= 0
    && px < width
    && py >= 0
    && py < height

search :: Int -> Int -> Map -> [State] -> S.Set State -> S.Set Pos
search _ _ _ [] visited = S.map fst visited
search w h m sts visited =
 search w h m sts' visited'
 where
  sts' = concatMap (filter (`S.notMember` visited) . step w h m) sts
  visited' = foldr S.insert visited sts'

solution1 :: String -> String
solution1 s =
 show $
  S.size $
   search w h m [start] (S.singleton start)
 where
  (w, h, m) = processInput s
  start = ((0, 0), (1, 0))

solution2 :: String -> String
solution2 s =
 show $
  maximum $
   map
    ( \x ->
       S.size $
        search w h m [x] (S.singleton x)
    )
    possibleStarts
 where
  (w, h, m) = processInput s
  possibleStarts =
   map (curry (, (0, -1)) 0) [0 .. w - 1]
    ++ map ((, (0, 1)) . (, 0)) [0 .. w - 1]
    ++ map (curry (, (1, 0)) 0) [0 .. h - 1]
    ++ map (curry (, (-1, 0)) 0) [0 .. h - 1]
