module Year2022.Day14 (solution1, solution2) where

import qualified Data.IntMap.Strict as M
import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Pos = (Int, Int)
type Map = M.IntMap (S.Set Int)

processInput :: String -> Map
processInput =
  foldl'
    ( \acc (x, y) ->
        M.insertWith
          S.union
          x
          (S.singleton y)
          acc
    )
    M.empty
    . concatMap getPoses
    . lines
 where
  getPoses = go . map getPos . splitOn " -> "
  getPos s =
    let nums = splitOn "," s
     in (read (head nums), read (nums !! 1))
  go [] = []
  go (p1@(x1, y1) : p2@(x2, y2) : ps)
    | p1 == p2 = go (p2 : ps)
    | x1 == x2 =
        p1 : go ((x1, y1 + dir y1 y2) : p2 : ps)
    | otherwise =
        p1 : go ((x1 + dir x1 x2, y1) : p2 : ps)
  go [p] = [p]
  dir a b = if a > b then -1 else 1

-- h is maximum height
-- negative number means no floor
step :: Int -> Map -> Pos -> (Maybe Pos, Map)
step h m (x, y) =
  if y' < 0
    then (Nothing, m)
    else (p, m')
 where
  maybeY = m M.!? x >>= S.lookupGT y
  y' = fromMaybe (if h < 0 then -1 else h) maybeY
  leftDown = (x - 1, y')
  rightDown = (x + 1, y')
  isIn (px, py) = case m M.!? px of
    Just s -> S.member py s || py == h
    Nothing -> py == h
  (p, m') =
    if isIn leftDown
      then
        if isIn rightDown
          then
            ( Just (x, y' - 1)
            , M.insertWith
                S.union
                x
                (S.singleton (y' - 1))
                m
            )
          else
            step h m rightDown
      else step h m leftDown

run :: Int -> Int -> Map -> Int
run h sandUnit m =
  let (p, m') = step h m (500, 0)
   in case p of
        Nothing -> sandUnit
        Just pos ->
          if pos == (500, 0)
            then sandUnit + 1
            else run h (sandUnit + 1) m'

solution1 :: String -> IO ()
solution1 = print . run (-1) 0 . processInput

solution2 :: String -> IO ()
solution2 s = print $ run (h + 2) 0 m
 where
  m = processInput s
  h =
    maximum
      . foldl1' S.union
      . map snd
      $ M.toList m
