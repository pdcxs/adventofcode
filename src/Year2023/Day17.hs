module Year2023.Day17 (solution1, solution2) where

import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Dir = (Int, Int)
type Score = Int
type Map = M.Map Pos Int
type State = (Pos, Dir, Score)
type Record = M.Map Score (S.Set State)

processInput :: String -> (Int, Int, Map)
processInput s = (w, h, m)
 where
  ls = lines s
  w = length (head ls)
  h = length ls
  m = parseMap 0 0 M.empty s

parseMap :: Int -> Int -> Map -> String -> Map
parseMap _ _ m "" = m
parseMap _ y m ('\n' : cs) =
 parseMap 0 (y + 1) m cs
parseMap x y m (c : cs) =
 parseMap (x + 1) y m' cs
 where
  m' = M.insert (x, y) (digitToInt c) m

search ::
 Int ->
 Int ->
 Map ->
 Int ->
 Int ->
 Record ->
 S.Set State ->
 Int
search w h m minStep maxStep q visited
 | pos == (w - 1, h - 1) && step >= minStep = sc
 | otherwise = search w h m minStep maxStep q' visited'
 where
  r = M.deleteFindMin q
  (sc, best, q') = getBest w h m minStep maxStep visited r
  (pos, _, step) = best
  visited' = S.insert best visited

getBest ::
 Int ->
 Int ->
 Map ->
 Int ->
 Int ->
 S.Set State ->
 ( (Score, S.Set State)
 , Record
 ) ->
 (Score, State, Record)
getBest w h m minStep maxStep visited ((sc, sts), q) =
 (sc, best, q'')
 where
  (best, sts') = S.deleteFindMin sts
  nexts = getNexts w h m minStep maxStep sc visited best
  q' = if null sts' then q else M.insert sc sts' q
  q'' = foldr merge q' nexts

merge :: (Score, State) -> Record -> Record
merge (sc, st) rcd = case oldScores of
 [] -> M.insertWith S.union sc (S.singleton st) rcd
 (os : _) ->
  if os <= sc
   then rcd
   else
    let oldSts = rcd M.! os
        oldSts' = S.delete st oldSts
     in M.insertWith
         S.union
         sc
         (S.singleton st)
         (M.insert os oldSts' rcd)
 where
  oldScores =
   [ s
   | (s, sts) <- M.toList rcd
   , st `S.member` sts
   ]

getNexts ::
 Int ->
 Int ->
 Map ->
 Int ->
 Int ->
 Score ->
 S.Set State ->
 State ->
 [(Score, State)]
getNexts
 w
 h
 m
 minStep
 maxStep
 sc
 visited
 ((x, y), (dx, dy), step) =
  filter (\(_, st) -> st `S.notMember` visited) $
   forward ++ left ++ right
  where
   valid (px, py) = px >= 0 && px < w && py >= 0 && py < h
   forward =
    let px = x + dx
        py = y + dy
        pos = (px, py)
        sc' = sc + m M.! pos
     in [ (sc', (pos, (dx, dy), step + 1))
        | valid pos && step < maxStep
        ]
   left =
    let px = x - dy
        py = y + dx
        pos = (px, py)
        sc' = sc + m M.! pos
     in [ (sc', (pos, (-dy, dx), 1))
        | valid pos && step <= maxStep && step >= minStep
        ]
   right =
    let px = x + dy
        py = y - dx
        pos = (px, py)
        sc' = sc + m M.! pos
     in [ (sc', (pos, (dy, -dx), 1))
        | valid pos && step <= maxStep && step >= minStep
        ]

solve :: Int -> Int -> String -> Int
solve minStep maxStep s =
  search w h m minStep maxStep start S.empty
 where
  (w, h, m) = processInput s
  start =
   M.fromList
    [ (0, S.singleton ((0, 0), (1, 0), 0))
    , (0, S.singleton ((0, 0), (0, 1), 0))
    ]

solution1 :: String -> IO ()
solution1 = print . solve 0 3

solution2 :: String -> IO ()
solution2 = print . solve 4 10
