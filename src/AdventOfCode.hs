module AdventOfCode (solutions, animations) where

import qualified Data.Map as M
import qualified Year2024.Day1
import qualified Year2024.Day10
import qualified Year2024.Day11
import qualified Year2024.Day12
import qualified Year2024.Day13
import qualified Year2024.Day14
import qualified Year2024.Day15
import qualified Year2024.Day16
import qualified Year2024.Day2
import qualified Year2024.Day3
import qualified Year2024.Day4
import qualified Year2024.Day5
import qualified Year2024.Day6
import qualified Year2024.Day7
import qualified Year2024.Day8
import qualified Year2024.Day9

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions =
  M.fromList
    [ ((2024, 1, 1), Year2024.Day1.solution1)
    , ((2024, 1, 2), Year2024.Day1.solution2)
    , ((2024, 2, 1), Year2024.Day2.solution1)
    , ((2024, 2, 2), Year2024.Day2.solution2)
    , ((2024, 3, 1), Year2024.Day3.solution1)
    , ((2024, 3, 2), Year2024.Day3.solution2)
    , ((2024, 4, 1), Year2024.Day4.solution1)
    , ((2024, 4, 2), Year2024.Day4.solution2)
    , ((2024, 5, 1), Year2024.Day5.solution1)
    , ((2024, 5, 2), Year2024.Day5.solution2)
    , ((2024, 6, 1), Year2024.Day6.solution1)
    , ((2024, 6, 2), Year2024.Day6.solution2)
    , ((2024, 7, 1), Year2024.Day7.solution1)
    , ((2024, 7, 2), Year2024.Day7.solution2)
    , ((2024, 8, 1), Year2024.Day8.solution1)
    , ((2024, 8, 2), Year2024.Day8.solution2)
    , ((2024, 9, 1), Year2024.Day9.solution1)
    , ((2024, 9, 2), Year2024.Day9.solution2)
    , ((2024, 10, 1), Year2024.Day10.solution1)
    , ((2024, 10, 2), Year2024.Day10.solution2)
    , ((2024, 11, 1), Year2024.Day11.solution1)
    , ((2024, 11, 2), Year2024.Day11.solution2)
    , ((2024, 12, 1), Year2024.Day12.solution1)
    , ((2024, 12, 2), Year2024.Day12.solution2)
    , ((2024, 13, 1), Year2024.Day13.solution1)
    , ((2024, 13, 2), Year2024.Day13.solution2)
    , ((2024, 14, 1), Year2024.Day14.solution1)
    , ((2024, 14, 2), Year2024.Day14.solution2)
    , ((2024, 15, 1), Year2024.Day15.solution1)
    , ((2024, 15, 2), Year2024.Day15.solution2)
    , ((2024, 16, 1), Year2024.Day16.solution1)
    , ((2024, 16, 2), Year2024.Day16.solution2)
    ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations =
  M.fromList
    [ ((2024, 15, 3), Year2024.Day15.animation1)
    , ((2024, 15, 4), Year2024.Day15.animation2)
    , ((2024, 16, 3), Year2024.Day16.animation1)
    ]
