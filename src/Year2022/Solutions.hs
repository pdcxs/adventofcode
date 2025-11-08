module Year2022.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2022.Day01
import qualified Year2022.Day02
import qualified Year2022.Day03
import qualified Year2022.Day04
import qualified Year2022.Day05
import qualified Year2022.Day06
import qualified Year2022.Day07
import qualified Year2022.Day08
import qualified Year2022.Day09
import qualified Year2022.Day10

solutions ::
  M.Map (Int, Int, Int) (String -> IO ())
solutions =
  M.fromList
    [ ((2022, 1, 1), Year2022.Day01.solution1)
    , ((2022, 1, 2), Year2022.Day01.solution2)
    , ((2022, 2, 1), Year2022.Day02.solution1)
    , ((2022, 2, 2), Year2022.Day02.solution2)
    , ((2022, 3, 1), Year2022.Day03.solution1)
    , ((2022, 3, 2), Year2022.Day03.solution2)
    , ((2022, 4, 1), Year2022.Day04.solution1)
    , ((2022, 4, 2), Year2022.Day04.solution2)
    , ((2022, 5, 1), Year2022.Day05.solution1)
    , ((2022, 5, 2), Year2022.Day05.solution2)
    , ((2022, 6, 1), Year2022.Day06.solution1)
    , ((2022, 6, 2), Year2022.Day06.solution2)
    , ((2022, 7, 1), Year2022.Day07.solution1)
    , ((2022, 7, 2), Year2022.Day07.solution2)
    , ((2022, 8, 1), Year2022.Day08.solution1)
    , ((2022, 8, 2), Year2022.Day08.solution2)
    , ((2022, 9, 1), Year2022.Day09.solution1)
    , ((2022, 9, 2), Year2022.Day09.solution2)
    , ((2022, 10, 1), Year2022.Day10.solution1)
    , ((2022, 10, 2), Year2022.Day10.solution2)
    ]

animations ::
  M.Map
    (Int, Int, Int)
    (String -> [String])
animations = M.empty
