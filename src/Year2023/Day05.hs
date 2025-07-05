module Year2023.Day05 (solution1, solution2) where

import Data.Containers.ListUtils (nubOrd)
import Data.List.Split (splitWhen)

processInput ::
 String ->
 ([Int], [[(Int, Int, Int)]])
processInput s = (seeds, nums)
 where
  ls = lines s
  seeds = map read . tail . words $ head ls
  maps = map tail . splitWhen null $ drop 2 ls
  getNum =
   (\xs -> (head xs, xs !! 1, last xs))
    . map read
    . words
  nums = map (map getNum) maps

combineFunc ::
 (Int, Int, Int) ->
 (Int -> Int) ->
 (Int -> Int)
combineFunc (x, y, r) f n =
 if n < y || n >= y + r
  then
   f n
  else n - y + x

combineFuncs ::
 [[(Int, Int, Int)]] ->
 Int ->
 Int
combineFuncs maps = go fs
 where
  fs = map (foldr combineFunc id) maps
  go [] = id
  go (f' : fs') = go fs' . f'

solution1 :: String -> String
solution1 s =
 show . minimum $
  map f seeds
 where
  (seeds, maps) = processInput s
  f = combineFuncs maps

solution2 :: String -> String
solution2 s =
 show . fst . minimum $
  go maps eds
 where
  (seed, maps) = processInput s
  eds = getEnds seed
  getEnds (x : y : xs) =
   (x, x + y - 1)
    : getEnds xs
  getEnds [] = []
  getEnds _ = undefined
  -- merge :: new original
  merge (x2, y2) (x1, y1)
   | x1 > y2 || y1 < x2 = [(x1, y1)]
   | x1 >= x2 && y1 <= y2 = [(x1, y1)]
   | x1 <= x2 && y1 >= y2 =
     filter
      valid
      [ (x1, x2 - 1)
      , (x2, y2)
      , (y2 + 1, y1)
      ]
   | x1 <= x2 && y1 <= y2 =
     filter
      valid
      [ (x1, x2 - 1)
      , (x2, y1)
      ]
   | x1 >= x2 && y1 >= y2 =
     filter
      valid
      [ (x1, y2)
      , (y2 + 1, y1)
      ]
   | otherwise = undefined
  go (ms : mss) ends =
   go mss ends'
   where
    ends' = map apply $ go' ms ends
    go' [] e = e
    go' ((_, a, b) : mm) e =
     go'
      mm
      ( nubOrd $
         concatMap
          (merge (a, a + b - 1))
          e
      )
    f = foldr combineFunc id ms
    apply (a, b) = (f a, f b)
  go [] ends = ends
  valid = uncurry (<)
