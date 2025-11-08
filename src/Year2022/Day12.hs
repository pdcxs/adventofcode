module Year2022.Day12 (solution1, solution2) where

import Data.Char (ord)
import Data.List (sortOn)
import qualified Data.Map as M

type Pos = (Int, Int)
type Map = M.Map Pos Int
type Record = M.Map Pos Int -- shortest length

processInput :: String -> (Pos, Pos, Map)
processInput =
  parseMap (0, 0) (0, 0) 0 0 M.empty . lines

parseMap ::
  Pos ->
  Pos ->
  Int ->
  Int ->
  Map ->
  [String] ->
  (Pos, Pos, Map)
parseMap s e _ _ m [] = (s, e, m)
parseMap s e _ y m ("" : ss) =
  parseMap s e 0 (y + 1) m ss
parseMap _ e x y m (('S' : cs) : ss) =
  parseMap
    (x, y)
    e
    (x + 1)
    y
    (M.insert (x, y) 0 m)
    (cs : ss)
parseMap s _ x y m (('E' : cs) : ss) =
  parseMap
    s
    (x, y)
    (x + 1)
    y
    (M.insert (x, y) 25 m)
    (cs : ss)
parseMap s e x y m ((c : cs) : ss) =
  parseMap
    s
    e
    (x + 1)
    y
    (M.insert (x, y) (ord c - ord 'a') m)
    (cs : ss)

getNext :: Map -> Pos -> Record -> Record
getNext m (x, y) rcd =
  M.fromList
    (map (,dist) poss)
 where
  poss =
    [ p
    | p <-
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        ]
    , M.member p m
    , not (M.member p rcd)
    , m M.! p - m M.! (x, y) < 2
    ]
  dist = rcd M.! (x, y) + 1

selectBest :: Record -> Pos
selectBest =
  fst
    . head
    . sortOn snd
    . M.toList

mergeRecord :: Record -> Record -> Record
mergeRecord new old =
  foldl'
    ( \acc (p, d) ->
        case M.lookup p acc of
          Nothing -> M.insert p d acc
          Just d' ->
            if d' <= d
              then acc
              else M.insert p d acc
    )
    old
    (M.toList new)

search ::
  Pos -> Map -> Record -> Record -> Int
search e m candidates rcd
  | null candidates = maxBound
  | best == e = rcd' M.! e
  | otherwise = search e m cand' rcd'
 where
  best = selectBest candidates
  cand' =
    mergeRecord
      (getNext m best rcd')
      (M.delete best candidates)
  rcd' = M.insert best (candidates M.! best) rcd

solution1 :: String -> IO ()
solution1 input =
  print $
    search end m initCands initRcd
 where
  (start, end, m) = processInput input
  initRcd = M.fromList [(start, 0)]
  initCands = getNext m start initRcd

solution2 :: String -> IO ()
solution2 input = print . minimum $ dists
 where
  (_, e, m) = processInput input
  starts = [p | (p, h) <- M.toList m, h == 0]
  dists =
    map
      ( \p ->
          let initRcd = M.fromList [(p, 0)]
              initCands = getNext m p initRcd
           in search e m initCands initRcd
      )
      starts
