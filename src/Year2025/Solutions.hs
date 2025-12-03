module Year2025.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2025.Day01
import qualified Year2025.Day02
import qualified Year2025.Day03

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
    ]

animations ::
  M.Map
    (Int, Int, Int)
    (String -> [String])
animations = M.empty
