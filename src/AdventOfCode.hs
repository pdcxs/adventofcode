module AdventOfCode (solutions, animations) where

import qualified Data.Map.Strict as M

import qualified Year2023.Solutions
import qualified Year2024.Solutions

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions =
  foldr1
    M.union
    [ Year2023.Solutions.solutions
    , Year2024.Solutions.solutions
    ]

animations :: M.Map (Int, Int, Int) (String -> [String])
animations =
  foldr1
    M.union
    [ Year2023.Solutions.animations
    , Year2024.Solutions.animations
    ]
