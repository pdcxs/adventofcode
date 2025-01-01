module AdventOfCode (solutions) where

import qualified Data.Map as M
import qualified Year2024.Day1
import qualified Year2024.Day2
import qualified Year2024.Day3
import qualified Year2024.Day4
import qualified Year2024.Day5
import qualified Year2024.Day6
import qualified Year2024.Day7

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions =
  M.fromList
    [ ((2024, 1, 1), Year2024.Day1.solution1),
      ((2024, 1, 2), Year2024.Day1.solution2),
      ((2024, 2, 1), Year2024.Day2.solution1),
      ((2024, 2, 2), Year2024.Day2.solution2),
      ((2024, 3, 1), Year2024.Day3.solution1),
      ((2024, 3, 2), Year2024.Day3.solution2),
      ((2024, 4, 1), Year2024.Day4.solution1),
      ((2024, 4, 2), Year2024.Day4.solution2),
      ((2024, 5, 1), Year2024.Day5.solution1),
      ((2024, 5, 2), Year2024.Day5.solution2),
      ((2024, 6, 1), Year2024.Day6.solution1),
      ((2024, 6, 2), Year2024.Day6.solution2),
      ((2024, 7, 1), Year2024.Day7.solution1),
      ((2024, 7, 2), Year2024.Day7.solution2)
    ]