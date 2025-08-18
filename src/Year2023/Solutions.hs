module Year2023.Solutions (solutions, animations) where

import qualified Data.Map.Strict as M
import qualified Year2023.Day01
import qualified Year2023.Day02
import qualified Year2023.Day03
import qualified Year2023.Day04
import qualified Year2023.Day05
import qualified Year2023.Day06
import qualified Year2023.Day07
import qualified Year2023.Day08
import qualified Year2023.Day09
import qualified Year2023.Day10
import qualified Year2023.Day11
import qualified Year2023.Day12
import qualified Year2023.Day13
import qualified Year2023.Day14
import qualified Year2023.Day15
import qualified Year2023.Day16
import qualified Year2023.Day17
import qualified Year2023.Day18
import qualified Year2023.Day19
import qualified Year2023.Day20

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions =
  M.fromList
    [ ((2023, 1, 1), Year2023.Day01.solution1)
    , ((2023, 1, 2), Year2023.Day01.solution2)
    , ((2023, 2, 1), Year2023.Day02.solution1)
    , ((2023, 2, 2), Year2023.Day02.solution2)
    , ((2023, 3, 1), Year2023.Day03.solution1)
    , ((2023, 3, 2), Year2023.Day03.solution2)
    , ((2023, 4, 1), Year2023.Day04.solution1)
    , ((2023, 4, 2), Year2023.Day04.solution2)
    , ((2023, 5, 1), Year2023.Day05.solution1)
    , ((2023, 5, 2), Year2023.Day05.solution2)
    , ((2023, 6, 1), Year2023.Day06.solution1)
    , ((2023, 6, 2), Year2023.Day06.solution2)
    , ((2023, 7, 1), Year2023.Day07.solution1)
    , ((2023, 7, 2), Year2023.Day07.solution2)
    , ((2023, 8, 1), Year2023.Day08.solution1)
    , ((2023, 8, 2), Year2023.Day08.solution2)
    , ((2023, 9, 1), Year2023.Day09.solution1)
    , ((2023, 9, 2), Year2023.Day09.solution2)
    , ((2023, 10, 1), Year2023.Day10.solution1)
    , ((2023, 10, 2), Year2023.Day10.solution2)
    , ((2023, 11, 1), Year2023.Day11.solution1)
    , ((2023, 11, 2), Year2023.Day11.solution2)
    , ((2023, 12, 1), Year2023.Day12.solution1)
    , ((2023, 12, 2), Year2023.Day12.solution2)
    , ((2023, 13, 1), Year2023.Day13.solution1)
    , ((2023, 13, 2), Year2023.Day13.solution2)
    , ((2023, 14, 1), Year2023.Day14.solution1)
    , ((2023, 14, 2), Year2023.Day14.solution2)
    , ((2023, 15, 1), Year2023.Day15.solution1)
    , ((2023, 15, 2), Year2023.Day15.solution2)
    , ((2023, 16, 1), Year2023.Day16.solution1)
    , ((2023, 16, 2), Year2023.Day16.solution2)
    , ((2023, 17, 1), Year2023.Day17.solution1)
    , ((2023, 17, 2), Year2023.Day17.solution2)
    , ((2023, 18, 1), Year2023.Day18.solution1)
    , ((2023, 18, 2), Year2023.Day18.solution2)
    , ((2023, 19, 1), Year2023.Day19.solution1)
    , ((2023, 19, 2), Year2023.Day19.solution2)
    , ((2023, 20, 0), Year2023.Day20.solution0)
    , ((2023, 20, 1), Year2023.Day20.solution1)
    , ((2023, 20, 2), Year2023.Day20.solution2)
    ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations = M.empty
