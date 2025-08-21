{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2023.Day02 (solution1, solution2) where

import Data.List.Split
import qualified Data.Map.Strict as M

type Round = M.Map String Int

processLine :: String -> (Int, Round)
processLine s =
 ( gameId
 , foldr (M.unionWith max) initRnd rounds
 )
 where
  [g, rds] = splitOn ": " s
  gameId = read (drop 5 g)
  rounds = map processRound (splitOn "; " rds)
  initRnd =
   M.fromList
    [("red", 0), ("green", 0), ("blue", 0)]
  processRound rd =
   M.fromList $
    map
     ( \r ->
        let [n, c] = splitOn " " r
         in (c, read n)
     )
     (splitOn ", " rd)

solution1 :: String -> IO ()
solution1 s =
 print . sum $
  [ gameId
  | l <- lines s
  , let (gameId, rd) = processLine l
  , isValid rd
  ]
 where
  isValid r =
   r M.! "red" <= 12
    && r M.! "green" <= 13
    && r M.! "blue" <= 14

solution2 :: String -> IO ()
solution2 s =
 print . sum $
  [ product rd
  | l <- lines s
  , let (_, rd) = processLine l
  ]
