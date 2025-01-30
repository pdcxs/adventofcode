module Year2023.Day03 (solution1, solution2) where

import Control.Monad
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map.Strict as M

type Pos = (Int, Int)

type Map = M.Map Pos Char

parseMap :: Int -> Int -> Map -> String -> Map
parseMap _ _ m "" = m
parseMap _ y m ('\n' : cs) = parseMap 0 (y + 1) m cs
parseMap x y m ('.' : cs) = parseMap (x + 1) y m cs
parseMap x y m (c : cs) =
  parseMap (x + 1) y (M.insert (x, y) c m) cs

processInput :: String -> Map
processInput = parseMap 0 0 M.empty

digitStarts :: Map -> [Pos]
digitStarts m = do
  (p@(x, y), c) <- M.assocs m
  guard $ isDigit c
  let v = m M.!? (x - 1, y)
  case v of
    Nothing -> return p
    Just left -> do
      guard $ not (isDigit left)
      return p

getNumPos :: Map -> Pos -> [Pos]
getNumPos m = go []
 where
  go ps p@(x, y) = case m M.!? p of
    Nothing -> ps
    Just c ->
      if isDigit c
        then
          go (p : ps) (x + 1, y)
        else ps

gearNums :: Map -> M.Map Pos [Int]
gearNums m =
  M.fromListWith (++)
    . map
      ( \(c, i) ->
          (head c, [read $ reverse i])
      )
    . filter (not . null . fst)
    $ zip gears ints
 where
  ds = digitStarts m
  nums = map (getNumPos m) ds
  ints = map (map (m M.!)) nums
  ns = nubOrd $ map (concatMap neighbors) nums
  gears = map getGear ns
  getGear ps = do
    p <- ps
    case m M.!? p of
      Just c -> do
        guard $ not (isDigit c)
        return p
      Nothing -> []

neighbors :: Pos -> [Pos]
neighbors (x, y) =
  [ (x + dx, y + dy)
  | dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  , abs dx + abs dy /= 0
  ]

solution1 :: String -> String
solution1 =
  show
    . sum
    . concat
    . M.elems
    . gearNums
    . processInput

solution2 :: String -> String
solution2 s =
  show $
    sum
      [ product ints
      | (p, ints) <- M.assocs gn
      , m M.! p == '*'
      , length ints == 2
      ]
 where
  m = processInput s
  gn = gearNums m
