{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2023.Day24 (solution1, solution2) where

import Data.Foldable (for_)
import Data.List.Split (splitOn)
import Data.SBV

type Coord = Int
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
   x = fromIntegral x1 + fromIntegral vx1 * t1
   y = fromIntegral y1 + fromIntegral vy1 * t1
   den = fromIntegral (vx2 * vy1 - vx1 * vy2)
   n1 =
    fromIntegral
     (vy2 * x1 - vy2 * x2 - vx2 * y1 + vx2 * y2)
   n2 =
    fromIntegral
     (vy1 * x1 - vy1 * x2 - vx1 * y1 + vx1 * y2)

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

solveRock :: [(Point, Point)] -> IO (Maybe (AlgReal, AlgReal, AlgReal))
solveRock inputs = do
 res <- sat $ do
  x <- sReal "x"
  y <- sReal "y"
  z <- sReal "z"
  dx <- sReal "dx"
  dy <- sReal "dy"
  dz <- sReal "dz"

  for_ inputs $ \((x_, y_, z_), (dx_, dy_, dz_)) -> do
   t <- sReal "t"
   constrain (t * (dx - fromIntegral dx_) .== (fromIntegral x_ - x))
   constrain (t * (dy - fromIntegral dy_) .== (fromIntegral y_ - y))
   constrain (t * (dz - fromIntegral dz_) .== (fromIntegral z_ - z))

   return (x, y, z, dx, dy, dz)
 case res of
  SatResult m -> case ( getModelValue "x" m
                      , getModelValue "y" m
                      , getModelValue "z" m
                      ) of
   (Just x, Just y, Just z) -> return $ Just (x, y, z)
   _ -> return Nothing

solution2 :: String -> IO ()
solution2 input = do
   let parsed = processInput input
   result <- solveRock (take 3 parsed)
   case result of
      Just (x, y, z) -> print $ x + y + z
      Nothing -> putStrLn "No solution found"