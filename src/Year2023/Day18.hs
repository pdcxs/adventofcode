module Year2023.Day18 (solution1, solution2) where

import Common.Utils (safeHead)
import Data.Text.Internal.Read (hexDigitToInt)

type Pos = (Int, Int)
type Dir = (Int, Int)
type Color = String

processInput :: String -> [(Dir, Int, Color)]
processInput s = map procLine (lines s)
 where
  procLine ln =
    let xs = words ln
        d = safeHead xs
        step = xs !! 1
        color = xs !! 2
     in (getDir d, read step, init $ drop 2 color)
  getDir "U" = (0, -1)
  getDir "R" = (1, 0)
  getDir "L" = (-1, 0)
  getDir "D" = (0, 1)
  getDir _ = undefined

getCoords :: [(Dir, Int, Color)] -> [Pos]
getCoords = go [(0, 0)]
 where
  go ps [] = ps
  go ps@((x, y) : _) (((dx, dy), size, _) : rs) =
    let x' = x + dx * size
        y' = y + dy * size
     in go ((x', y') : ps) rs
  go _ _ = undefined

getArea :: [Pos] -> Int
getArea xs = floor $ getCirc xs / 2 + 1 + go 0 xs
 where
  go ar ((x1, y1) : (x2, y2) : rs) =
    go (x1 * y2 - x2 * y1 + ar) ((x2, y2) : rs)
  go ar _ = fromIntegral (abs ar) / 2

getCirc :: [Pos] -> Double
getCirc ((x1, y1) : (x2, y2) : rs) =
  fromIntegral (abs (x1 - x2) + abs (y1 - y2))
    + getCirc ((x2, y2) : rs)
getCirc _ = 0

solution1 :: String -> String
solution1 = show . getArea . getCoords . processInput

hex :: String -> Int
hex =
  fst
    . foldr
      ( \e (val, base) ->
          (val + base * hexDigitToInt e, base * 16)
      )
      (0, 1)

getCoords' :: [(Dir, Int, Color)] -> [Pos]
getCoords' = go [(0, 0)]
 where
  go ps [] = ps
  go ps@((x, y) : _) ((_, _, color) : rs) =
    let ((dx, dy), size) = parse color
        x' = x + dx * size
        y' = y + dy * size
     in go ((x', y') : ps) rs
  go _ _ = undefined

parse :: String -> (Dir, Int)
parse s = (getDir s2, hex s1)
 where
  (s1, s2) = splitAt 5 s
  getDir "0" = (1, 0)
  getDir "1" = (0, 1)
  getDir "2" = (-1, 0)
  getDir "3" = (0, -1)
  getDir _ = undefined

solution2 :: String -> String
solution2 = show . getArea . getCoords' . processInput
