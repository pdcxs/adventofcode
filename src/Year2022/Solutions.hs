module Year2022.Solutions (solutions, animations) where

import qualified Data.Map as M
import qualified Year2022.Day01
import qualified Year2022.Day02
import qualified Year2022.Day03

solutions :: M.Map (Int, Int, Int) (String -> IO ())
solutions =
 M.fromList
  [ ((2022, 1, 1), Year2022.Day01.solution1)
  , ((2022, 1, 2), Year2022.Day01.solution2)
  , ((2022, 2, 1), Year2022.Day02.solution1)
  , ((2022, 2, 2), Year2022.Day02.solution2)
  , ((2022, 3, 1), Year2022.Day03.solution1)
  , ((2022, 3, 2), Year2022.Day03.solution2)
  ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations = M.empty
