module Year2023.Solutions (solutions, animations) where

import qualified Data.Map.Strict as M
import qualified Year2023.Day01

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions =
  M.fromList
    [ ((2023, 1, 1), Year2023.Day01.solution1)
    , ((2023, 1, 2), Year2023.Day01.solution2)
    ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations = M.empty
