module Year2024.Day20 (solution1, solution2) where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Walls = S.Set Pos

-- position and its distance from start
type Path = M.Map Pos Int

processInput ::
  String -> (Walls, Pos, Pos)
processInput = go S.empty [] [] 0 0
 where
  go walls start end x y ('#' : ss) =
    go
      (S.insert (x, y) walls)
      start
      end
      (x + 1)
      y
      ss
  go walls start end _ y ('\n' : ss) =
    go walls start end 0 (y + 1) ss
  go walls _ end x y ('S' : ss) =
    go walls [(x, y)] end (x + 1) y ss
  go walls start _ x y ('E' : ss) =
    go walls start [(x, y)] (x + 1) y ss
  go walls start end _ _ [] = (walls, head start, head end)
  go walls start end x y (_ : ss) =
    go walls start end (x + 1) y ss

getPath :: Walls -> Pos -> Pos -> Path
getPath walls start end =
  go start next 1 (M.singleton start 0)
 where
  next = getNext start start
  getNext prev p =
    head
      [ n
      | n <- neighbors p
      , n /= prev
      , n `S.notMember` walls
      ]
  go p n d record
    | n == end = record'
    | otherwise = go n n' (d + 1) record'
   where
    record' = M.insert n d record
    n' = getNext p n

getCheatDest ::
  Int -> -- map width
  Int -> -- map height
  Path -> -- path
  Int -> -- cheat maximum time
  Pos -> -- current position
  [(Pos, Int)] -- candidate dest positions and moved distance
getCheatDest width height path dist (px, py) =
  [ (p, d)
  | (p, d) <- cheatRange
  , p `M.member` path
  ]
 where
  cheatRange = do
    x <- [px - dist .. px + dist]
    guard (x > 0 && x < width)
    y <- [py - dist .. py + dist]
    guard (y > 0 && y < height)
    let d = abs (x - px) + abs (y - py)
    guard (d <= dist)
    return ((x, y), d)

getCheatPlan ::
  Int ->
  Int ->
  Path ->
  Int ->
  M.Map (Pos, Pos) Int
getCheatPlan width height path dist =
  M.fromList $ do
    (from, d) <- M.assocs path
    (to, moved) <-
      getCheatDest width height path dist from
    let dist' = path M.! to
    guard (dist' > dist - moved)
    return ((from, to), dist' - d - moved)

neighbors :: Pos -> [Pos]
neighbors (x, y) =
  [ (x - 1, y)
  , (x + 1, y)
  , (x, y - 1)
  , (x, y + 1)
  ]

solution1 :: String -> IO ()
solution1 s = print (M.size goodPlans)
 where
  (walls, start, end) = processInput s
  (width, height) = maximum walls
  path = getPath walls start end
  plans = getCheatPlan width height path 2
  goodPlans = M.filter (>= 100) plans

solution2 :: String -> IO ()
solution2 s = print (M.size goodPlans)
 where
  (walls, start, end) = processInput s
  (width, height) = maximum walls
  path = getPath walls start end
  plans = getCheatPlan width height path 20
  goodPlans = M.filter (>= 100) plans
