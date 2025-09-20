{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2023.Day24 (solution1, solution2) where

import Data.List.Split (splitOn)
import Linear.V4 ( V4(..) )
import Linear.Matrix ( (!*), inv44, M44 )

type Coord = Double
type Point = (Coord, Coord, Coord)

processInput :: String -> [(Point, Point)]
processInput = map parseLine . lines
 where
  getPoint s =
   let [x, y, z] = map read (splitOn ", " s)
    in (x, y, z)
  parseLine line =
   let [l, r] = splitOn " @ " line
       lp = getPoint l
       rp = getPoint r
    in (lp, rp)

getResult ::
 (Point, Point) ->
 (Point, Point) ->
 Maybe (Double, Double)
getResult
 ((x1, y1, _), (vx1, vy1, _))
 ((x2, y2, _), (vx2, vy2, _))
  | vx1 * vy2 == vx2 * vy1 = Nothing
  | t1 < 0 || t2 < 0 = Nothing
  | otherwise = Just (x, y)
  where
   t1 = n1 / den
   t2 = n2 / den
   x = x1 + vx1 * t1
   y = y1 + vy1 * t1
   den = vx2 * vy1 - vx1 * vy2
   n1 = vy2 * x1 - vy2 * x2 - vx2 * y1 + vx2 * y2
   n2 = vy1 * x1 - vy1 * x2 - vx1 * y1 + vx1 * y2

isValid ::
 (Double, Double) ->
 Maybe (Double, Double) ->
 Bool
isValid _ Nothing = False
isValid (low, high) (Just (x, y)) =
 x >= low
  && x <= high
  && y >= low
  && y <= high

solution1 :: String -> IO ()
solution1 input = print (length validResults)
 where
  parsed = processInput input
  results =
   [ getResult p1 p2
   | p1 <- parsed
   , p2 <- parsed
   , p1 < p2
   ]
  validResults =
   [ r
   | r <- results
   , isValid
      ( 200000000000000
      , 400000000000000
      )
      r
   ]

-- Part 2
-- Assume rock position and velocity are:
-- (x0, y0, z0), (vx0, vy0, vz0)
-- we first consider x and y
-- at time ti, the rock collides with
-- hailstone i (i <- [1 .. n]), so
-- x0 + vx0 * ti = xi + vxi * ti (1)
-- y0 + vy0 * ti = yi + vyi * ti (2)
-- ti is different for each i
-- we need to delete ti
-- from (1) we can get
-- (vx0 - vxi) * ti = xi - x0 (assume vx0 /= vxi)
-- ti = (xi - x0) / (vx0 - vxi)
-- from (2) we can also get
-- ti = (yi - y0) / (vy0 - vyi)
-- so (xi - x0) / (vx0 - vxi) = (yi - y0) / (vy0 - vyi)
-- (xi - x0) * (vy0 - vyi) = (yi - y0) * (vx0 - vxi)
-- -vy0 x0 + vyi x0 + vy0 xi - vyi xi + vx0 y0 - vxi y0 - vx0 yi + vxi yi = 0
-- But we have -vy0 * x0 which should be deleted
-- let i = 1 we get:
-- -vy0 x0 + vy1 x0 + vy0 x1 - vy1 x1 + vx0 y0 - vx1 y0 - vx0 y1 + vx1 y1 (3)
-- let i = 2 we get:
-- -vy0 x0 + vy2 x0 + vy0 x2 - vy2 x2 + vx0 y0 - vx2 y0 - vx0 y2 + vx2 y2 (4)
-- (3) - (4) we get:
-- (vy1 - vy2) x0 + (vx2 - vx1) y0 + (y2 - y1) vx0 + (x1 - x2) vy0 =
-- vy1 x1 - vy2 x2 - vx1 y1 + vx2 y2 
-- change 2 to 3, 4 and 5, we will get 4 equations for 4 unknowns
-- change y to z, use the same method, we can get z0 and vz0

getXY :: [(Point, Point)] -> (M44 Double, V4 Double)
getXY (
   ((x1, y1, _), (vx1, vy1, _)) :
   ((x2, y2, _), (vx2, vy2, _)) :
   ((x3, y3, _), (vx3, vy3, _)) :
   ((x4, y4, _), (vx4, vy4, _)) :
   ((x5, y5, _), (vx5, vy5, _)) : _) =
      (
      V4 
         (V4 (vy1 - vy2) (vx2 - vx1) (y2 - y1) (x1 - x2))
         (V4 (vy1 - vy3) (vx3 - vx1) (y3 - y1) (x1 - x3))
         (V4 (vy1 - vy4) (vx4 - vx1) (y4 - y1) (x1 - x4))
         (V4 (vy1 - vy5) (vx5 - vx1) (y5 - y1) (x1 - x5))
      ,
      V4 (vy1 * x1 - vy2 * x2 - vx1 * y1 + vx2 * y2)
         (vy1 * x1 - vy3 * x3 - vx1 * y1 + vx3 * y3)
         (vy1 * x1 - vy4 * x4 - vx1 * y1 + vx4 * y4)
         (vy1 * x1 - vy5 * x5 - vx1 * y1 + vx5 * y5)
      )
getXY _ = error "Not enough data"

getXZ :: [(Point, Point)] -> (M44 Double, V4 Double)
getXZ (
   ((x1, _, z1), (vx1, _, vz1)) :
   ((x2, _, z2), (vx2, _, vz2)) :
   ((x3, _, z3), (vx3, _, vz3)) :
   ((x4, _, z4), (vx4, _, vz4)) :
   ((x5, _, z5), (vx5, _, vz5)) : _) =
      (
      V4 
         (V4 (vz1 - vz2) (vx2 - vx1) (z2 - z1) (x1 - x2))
         (V4 (vz1 - vz3) (vx3 - vx1) (z3 - z1) (x1 - x3))
         (V4 (vz1 - vz4) (vx4 - vx1) (z4 - z1) (x1 - x4))
         (V4 (vz1 - vz5) (vx5 - vx1) (z5 - z1) (x1 - x5))
      ,
      V4 (vz1 * x1 - vz2 * x2 - vx1 * z1 + vx2 * z2)
         (vz1 * x1 - vz3 * x3 - vx1 * z1 + vx3 * z3)
         (vz1 * x1 - vz4 * x4 - vx1 * z1 + vx4 * z4)
         (vz1 * x1 - vz5 * x5 - vx1 * z1 + vx5 * z5)
      )
getXZ _ = error "Not enough data"

solution2 :: String -> IO ()
solution2 input = do
 let parsed = processInput input
     (mx, vx) = getXY parsed
     (mz, vz) = getXZ parsed
     V4 x0 y0 _ _ = inv44 mx !* vx
     V4 _ z0 _ _ = inv44 mz !* vz
 print (round (x0 + y0 + z0) :: Integer)