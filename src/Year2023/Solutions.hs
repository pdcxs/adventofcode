module Year2023.Solutions (solutions, animations) where

import qualified Data.Map.Strict as M
import qualified Year2023.Day01
import qualified Year2023.Day02
import qualified Year2023.Day03
import qualified Year2023.Day04

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
    ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations = M.empty
