{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2025.Day10 (solution1, solution2) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Data.SBV

type Problem = ([Bool], [[Int]], [Int])
type ContraintFunc =
  Int -> Problem -> [SInteger] -> SBool

processInput :: String -> [Problem]
processInput = map parse . lines
 where
  parse s =
    let xs = words s
     in ( map (== '#') (init . drop 1 $ head xs)
        , map getNums (init $ drop 1 xs)
        , getNums (last xs)
        )
  getNums = map read . splitOn "," . init . drop 1

getConstraint :: ContraintFunc
getConstraint idx (sts, btns, _) xs =
  left `sMod` 2 .== (if s then 1 else 0)
 where
  s = sts !! idx
  left =
    sum . map fst $
      filter (\(_, bs) -> idx `elem` bs) (zip xs btns)

getProblem :: ContraintFunc -> Problem -> Symbolic ()
getProblem cf prob@(finalStates, buttons, _) = do
  let n = length buttons
      btns = [0 .. n - 1]
  xs <- mapM (sInteger . ('x' :) . show) btns
  constrain $ sAll (.>= 0) xs
  let cs =
        map
          (\idx -> cf idx prob xs)
          [0 .. length finalStates - 1]
  constrain $ sAnd cs
  minimize "r" (sum xs)

getResult :: Symbolic () -> IO Integer
getResult problem = do
  opr <- optimize Lexicographic problem
  let LexicographicResult r = opr
  let x = getModelValue "r" r
  return (fromJust x)

solveProb :: ContraintFunc -> String -> IO ()
solveProb cf s = do
  let ps = map (getProblem cf) $ processInput s
  rs <- mapM getResult ps
  print $ sum rs

solution1 :: String -> IO ()
solution1 = solveProb getConstraint

getConstraint' :: ContraintFunc
getConstraint' idx (_, btns, levels) xs =
  let target = fromIntegral $ levels !! idx
   in sum
        ( map
            snd
            (filter (\(b, _) -> idx `elem` b) (zip btns xs))
        )
        .== target

solution2 :: String -> IO ()
solution2 = solveProb getConstraint'
