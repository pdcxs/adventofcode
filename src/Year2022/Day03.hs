{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}

module Year2022.Day03 (solution1, solution2) where

import Data.Char (isLower, ord)
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import qualified Data.Set as S

getPriority :: Char -> Int
getPriority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

solution1 :: String -> IO ()
solution1 =
  print
    . sum
    . map
      ( \l ->
          sum
            . S.map getPriority
            . foldl1' S.intersection
            . map S.fromList
            . chunksOf (length l `div` 2)
            $ l
      )
    . filter (not . null)
    . lines

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . map
      ( sum
          . S.map getPriority
          . foldl1' S.intersection
          . map S.fromList
      )
    . chunksOf 3
    . filter (not . null)
    . lines
