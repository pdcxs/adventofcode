module Year2024.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2024.Day01
import qualified Year2024.Day02
import qualified Year2024.Day03
import qualified Year2024.Day04
import qualified Year2024.Day05
import qualified Year2024.Day06
import qualified Year2024.Day07
import qualified Year2024.Day08
import qualified Year2024.Day09
import qualified Year2024.Day10
import qualified Year2024.Day11
import qualified Year2024.Day12
import qualified Year2024.Day13
import qualified Year2024.Day14
import qualified Year2024.Day15
import qualified Year2024.Day16
import qualified Year2024.Day17
import qualified Year2024.Day18
import qualified Year2024.Day19
import qualified Year2024.Day20
import qualified Year2024.Day21
import qualified Year2024.Day22
import qualified Year2024.Day23
import qualified Year2024.Day24
import qualified Year2024.Day25

solutions ::
  M.Map (Int, Int, Int) (String -> IO ())
solutions =
  M.fromList
    [ ((2024, 1, 1), Year2024.Day01.solution1)
    , ((2024, 1, 2), Year2024.Day01.solution2)
    , ((2024, 2, 1), Year2024.Day02.solution1)
    , ((2024, 2, 2), Year2024.Day02.solution2)
    , ((2024, 3, 1), Year2024.Day03.solution1)
    , ((2024, 3, 2), Year2024.Day03.solution2)
    , ((2024, 4, 1), Year2024.Day04.solution1)
    , ((2024, 4, 2), Year2024.Day04.solution2)
    , ((2024, 5, 1), Year2024.Day05.solution1)
    , ((2024, 5, 2), Year2024.Day05.solution2)
    , ((2024, 6, 1), Year2024.Day06.solution1)
    , ((2024, 6, 2), Year2024.Day06.solution2)
    , ((2024, 7, 1), Year2024.Day07.solution1)
    , ((2024, 7, 2), Year2024.Day07.solution2)
    , ((2024, 8, 1), Year2024.Day08.solution1)
    , ((2024, 8, 2), Year2024.Day08.solution2)
    , ((2024, 9, 1), Year2024.Day09.solution1)
    , ((2024, 9, 2), Year2024.Day09.solution2)
    ,
      ( (2024, 10, 1)
      , Year2024.Day10.solution1
      )
    ,
      ( (2024, 10, 2)
      , Year2024.Day10.solution2
      )
    ,
      ( (2024, 11, 1)
      , Year2024.Day11.solution1
      )
    ,
      ( (2024, 11, 2)
      , Year2024.Day11.solution2
      )
    ,
      ( (2024, 12, 1)
      , Year2024.Day12.solution1
      )
    ,
      ( (2024, 12, 2)
      , Year2024.Day12.solution2
      )
    ,
      ( (2024, 13, 1)
      , Year2024.Day13.solution1
      )
    ,
      ( (2024, 13, 2)
      , Year2024.Day13.solution2
      )
    ,
      ( (2024, 14, 1)
      , Year2024.Day14.solution1
      )
    ,
      ( (2024, 14, 2)
      , Year2024.Day14.solution2
      )
    ,
      ( (2024, 15, 1)
      , Year2024.Day15.solution1
      )
    ,
      ( (2024, 15, 2)
      , Year2024.Day15.solution2
      )
    ,
      ( (2024, 16, 1)
      , Year2024.Day16.solution1
      )
    ,
      ( (2024, 16, 2)
      , Year2024.Day16.solution2
      )
    ,
      ( (2024, 17, 1)
      , Year2024.Day17.solution1
      )
    ,
      ( (2024, 17, 2)
      , Year2024.Day17.solution2
      )
    ,
      ( (2024, 18, 1)
      , Year2024.Day18.solution1
      )
    ,
      ( (2024, 18, 2)
      , Year2024.Day18.solution2
      )
    ,
      ( (2024, 19, 1)
      , Year2024.Day19.solution1
      )
    ,
      ( (2024, 19, 2)
      , Year2024.Day19.solution2
      )
    ,
      ( (2024, 20, 1)
      , Year2024.Day20.solution1
      )
    ,
      ( (2024, 20, 2)
      , Year2024.Day20.solution2
      )
    ,
      ( (2024, 21, 1)
      , Year2024.Day21.solution1
      )
    ,
      ( (2024, 21, 2)
      , Year2024.Day21.solution2
      )
    ,
      ( (2024, 22, 1)
      , Year2024.Day22.solution1
      )
    ,
      ( (2024, 22, 2)
      , Year2024.Day22.solution2
      )
    ,
      ( (2024, 23, 1)
      , Year2024.Day23.solution1
      )
    ,
      ( (2024, 23, 2)
      , Year2024.Day23.solution2
      )
    ,
      ( (2024, 24, 1)
      , Year2024.Day24.solution1
      )
    ,
      ( (2024, 24, 2)
      , Year2024.Day24.solution2
      )
    ,
      ( (2024, 25, 1)
      , Year2024.Day25.solution1
      )
    ,
      ( (2024, 25, 2)
      , Year2024.Day25.solution2
      )
    ]

animations ::
  M.Map
    (Int, Int, Int)
    (String -> [String])
animations =
  M.fromList
    [
      ( (2024, 15, 3)
      , Year2024.Day15.animation1
      )
    ,
      ( (2024, 15, 4)
      , Year2024.Day15.animation2
      )
    ,
      ( (2024, 17, 3)
      , Year2024.Day17.animation
      )
    ]
