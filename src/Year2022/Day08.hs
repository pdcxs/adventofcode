module Year2022.Day08 (solution1, solution2) where

import Data.Char (ord)
import qualified Data.Map.Strict as M

type Pos = (Int, Int)
type Map = M.Map Pos Int

parseMap ::
  Int -> Int -> Map -> [String] -> Map
parseMap _ _ m [] = m
parseMap _ y m ([] : ss) =
  parseMap 0 (y + 1) m ss
parseMap x y m ((c : cs) : ss) =
  parseMap (x + 1) y m' (cs : ss)
 where
  m' = M.insert (x, y) (ord c - ord '0') m

parseInput :: String -> Map
parseInput = parseMap 0 0 M.empty . lines

dirs :: [(Int, Int)]
dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

isVisible :: Map -> Pos -> Bool
isVisible m pos =
  any (go pos) dirs
 where
  height = m M.! pos
  go (x, y) (dx, dy) =
    let next = (x + dx, y + dy)
     in case M.lookup next m of
          Just h ->
            (h < height) && go next (dx, dy)
          Nothing -> True

solution1 :: String -> IO ()
solution1 input =
  print
    . length
    . filter (isVisible m)
    $ M.keys m
 where
  m = parseInput input

getVisibleCnt ::
  Map -> Pos -> (Int, Int) -> Int
getVisibleCnt m pos dir = go pos dir
 where
  height = m M.! pos
  go (x, y) (dx, dy) =
    let next = (x + dx, y + dy)
     in case M.lookup next m of
          Just h ->
            if h >= height
              then 1
              else 1 + go next (dx, dy)
          Nothing -> 0

getCount :: Map -> Pos -> Int
getCount m p =
  product $
    map (getVisibleCnt m p) dirs

solution2 :: String -> IO ()
solution2 input =
  print
    . maximum
    . map (getCount m)
    $ M.keys m
 where
  m = parseInput input
