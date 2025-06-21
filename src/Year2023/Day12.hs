module Year2023.Day12 (solution1, solution2) where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)

type Count = Int

processInput ::
  (String -> String) ->
  (String -> String) ->
  String ->
  [(String, [Count])]
processInput preProcess1 preProcess2 =
  map processLine . lines
 where
  processLine s =
    let (s1, s2) = span (/= ' ') s
        s1' = preProcess1 s1
        s2' = preProcess2 (drop 1 s2)
     in (s1', map read $ splitOn "," s2')

getAnswer :: (String, [Count]) -> Int
getAnswer = uncurry putIn
 where
  putIn :: String -> [Count] -> Int
  putIn cs [] =
    if any (== '#') cs then 0 else 1
  putIn [] [0] = 1
  putIn [] _ = 0
  putIn ('#' : _) (0 : _) = 0
  putIn (_ : cs) (0 : cts) = memo2 putIn cs cts
  putIn ('#' : cs) (c : cts)
    | d < c - 1 = 0
    | otherwise = case next of
        [] -> memo2 putIn [] cts
        ('#' : _) -> 0
        (_ : cs') -> memo2 putIn cs' cts
   where
    ns = takeWhile (\x -> x == '#' || x == '?') cs
    n = length ns
    d = min n (c - 1)
    next = drop d cs
  putIn ('.' : cs) cts = memo2 putIn cs cts
  putIn ('?' : []) [1] = 1
  putIn ('?' : []) _ = 0
  putIn ('?' : cs) (c : cts)
    | n < c - 1 = case next of
        ('#' : _) ->
          sum $
            map
              (\x -> memo2 putIn next ((c - x) : cts))
              [0 .. n + 1]
        _ -> memo2 putIn next (c : cts)
    | otherwise = case next of
        [] -> memo2 putIn [] cts
        ('?' : cs') ->
          memo2 putIn cs' cts + memo2 putIn cs (c : cts)
        ('#' : _) ->
          sum $
            map
              (\x -> memo2 putIn next (x : cts))
              [1 .. c]
        _ ->
          memo2 putIn (drop n cs) cts
            + memo2 putIn cs (c : cts)
   where
    ns = takeWhile (== '?') cs
    n = min (length ns) (c - 1)
    next = drop n cs
  putIn _ _ = undefined

solution1 :: String -> String
solution1 =
  show
    . sum
    . map getAnswer
    . processInput id id

solution2 :: String -> String
solution2 =
  show
    . sum
    . map getAnswer
    . processInput pre1 pre2
 where
  pre1 = timesWith "?"
  pre2 = timesWith ","
  timesWith c = concat . intersperse c . replicate 5
