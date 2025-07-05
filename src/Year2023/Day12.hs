module Year2023.Day12 (solution1, solution2) where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)

type Spring = Int
type Lava = String

processInput ::
 (String -> String) ->
 (String -> String) ->
 String ->
 [(Lava, [Spring])]
processInput preProcess1 preProcess2 =
 map processLine . lines
 where
  processLine s =
   let (s1, s2) = span (/= ' ') s
       s1' = preProcess1 s1
       s2' = preProcess2 (drop 1 s2)
    in (s1', map read $ splitOn "," s2')

solve :: Lava -> [Spring] -> Int
solve lava [] = if '#' `elem` lava then 0 else 1
solve lava sps@(s : ss) =
 sum $
  map
   ( \x ->
      memo2 solve (drop (x + 1) lava) ss
   )
   valid
 where
  n = length lava
  starts = [0 .. n - sum sps - length ss]
  checks =
   map
    ( \x ->
       (x + s, take s (drop x lava))
    )
    starts
  isValid (next, subLava) =
   let start = next - s
    in not $
        next > n
         || '#' `elem` take start lava
         || '.' `elem` subLava
         || startsWith '#' (drop next lava)
  valid = map fst $ filter isValid checks

startsWith :: (Eq a) => a -> [a] -> Bool
startsWith _ [] = False
startsWith x (y : _) = x == y

solution1 :: String -> String
solution1 =
 show
  . sum
  . map (uncurry solve)
  . processInput id id

solution2 :: String -> String
solution2 =
 show
  . sum
  . map (uncurry solve)
  . processInput pre1 pre2
 where
  pre1 = concat . intersperse "?" . replicate 5
  pre2 = concat . intersperse "," . replicate 5
