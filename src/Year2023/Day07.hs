module Year2023.Day07 (solution1, solution2) where

import Data.Char (digitToInt)
import Data.List (
  group,
  partition,
  sort,
  sortBy,
 )
import Data.Ord (Down (Down), comparing)

type Hand = [Char]

cmpInt :: Char -> Int
cmpInt 'A' = 14
cmpInt 'K' = 13
cmpInt 'Q' = 12
cmpInt 'J' = 11
cmpInt 'T' = 10
cmpInt c = digitToInt c

processInput :: String -> [(Hand, Int)]
processInput = map f . lines
 where
  f ln =
    let ws = words ln
     in ( head ws
        , read (last ws)
        )

handGroup :: [Int] -> [Int]
handGroup =
  sortBy (comparing Down)
    . map length
    . group
    . sort

getType :: [Int] -> Int
getType grp = case head grp of
  5 -> 7
  4 -> 6
  3 -> if grp !! 1 == 2 then 5 else 4
  2 -> if grp !! 1 == 2 then 3 else 2
  1 -> 1
  _ -> undefined

handToType :: Hand -> Int
handToType =
  getType
    . handGroup
    . map cmpInt

solution ::
  (Char -> Int) ->
  (Hand -> Int) ->
  String ->
  IO ()
solution cmpFunc typeFunc s =
  print $ sum prds
 where
  ins = processInput s
  rs =
    sort
      ( map
          ( \(hd, bid) ->
              ( typeFunc hd
              , map cmpFunc hd
              , bid
              )
          )
          ins
      )
  r =
    zip [1 ..] $
      map (\(_, _, bid) -> bid) rs
  prds = map (uncurry (*)) r

solution1 :: String -> IO ()
solution1 = solution cmpInt handToType

cmpInt' :: Char -> Int
cmpInt' 'A' = 13
cmpInt' 'K' = 12
cmpInt' 'Q' = 11
cmpInt' 'J' = 1
cmpInt' 'T' = 10
cmpInt' c = digitToInt c

handToType' :: Hand -> Int
handToType' hd = getType rs
 where
  (js, cs) = partition (== 'J') hd
  l = length js
  gps = handGroup (map cmpInt' cs)
  rs =
    if null gps
      then [l]
      else
        (head gps + l) : tail gps

solution2 :: String -> IO ()
solution2 = solution cmpInt' handToType'
