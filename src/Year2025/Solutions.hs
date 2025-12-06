module Year2025.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2025.Day01
import qualified Year2025.Day02
import qualified Year2025.Day03
import qualified Year2025.Day04
import qualified Year2025.Day05
import qualified Year2025.Day06

solutions ::
  M.Map (Int, Int, Int) (String -> IO ())
solutions =
  M.fromList
    [ ((2025, 1, 1), Year2025.Day01.solution1)
    , ((2025, 1, 2), Year2025.Day01.solution2)
    , ((2025, 2, 1), Year2025.Day02.solution1)
    , ((2025, 2, 2), Year2025.Day02.solution2)
    , ((2025, 3, 1), Year2025.Day03.solution1)
    , ((2025, 3, 2), Year2025.Day03.solution2)
    , ((2025, 4, 1), Year2025.Day04.solution1)
    , ((2025, 4, 2), Year2025.Day04.solution2)
    , ((2025, 5, 1), Year2025.Day05.solution1)
    , ((2025, 5, 2), Year2025.Day05.solution2)
    , ((2025, 6, 1), Year2025.Day06.solution1)
    , ((2025, 6, 2), Year2025.Day06.solution2)
    ]

animations ::
  M.Map
    (Int, Int, Int)
    (String -> [String])
animations = M.empty
