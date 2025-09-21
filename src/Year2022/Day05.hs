{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2022.Day05 (solution1, solution2) where

import Data.List (transpose)
import Data.List.Split (chunksOf, splitWhen)
import qualified Data.Map as M

type Crate = [Char]
type Crates = M.Map Int Crate
type Move = (Int, Int, Int) -- (count, from, to)

parseInput :: String -> (Crates, [Move])
parseInput input = (crates, moves)
 where
  [cratesInput, movesInput] = splitWhen null (lines input)
  cratesData =
   map
    ( map (last . take 2)
       . chunksOf 4
    )
    (init cratesInput)
  crates =
   M.fromList $
    zip [1 ..] $
     map (filter (/= ' ')) $
      transpose cratesData
  moves = map parseMove movesInput
  parseMove ln =
   let [_, count, _, from, _, to] = words ln
    in (read count, read from, read to)

moveCrates :: Bool -> Crates -> [Move] -> Crates
moveCrates _ cs [] = cs
moveCrates isR cs ((count, from, to) : ms) =
 moveCrates isR cs' ms
 where
  fromCrate = cs M.! from
  toCrate = cs M.! to
  cs' =
   let (moved, remain) = splitAt count fromCrate
       mvd = if isR then reverse moved else moved
    in M.insert from remain $
       M.insert to (mvd ++ toCrate) cs

solution :: Bool -> String -> IO ()
solution isR =
 putStrLn
  . map (head . snd)
  . M.toList
  . uncurry (moveCrates isR)
  . parseInput

solution1 :: String -> IO ()
solution1 = solution True

solution2 :: String -> IO ()
solution2 = solution False