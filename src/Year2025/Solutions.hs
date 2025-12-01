module Year2025.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2025.Day01

solutions ::
  M.Map (Int, Int, Int) (String -> IO ())
solutions =
  M.fromList
    [ ((2025, 1, 1), Year2025.Day01.solution1)
    , ((2025, 1, 2), Year2025.Day01.solution2)
    ]

animations ::
  M.Map
    (Int, Int, Int)
    (String -> [String])
animations = M.empty
