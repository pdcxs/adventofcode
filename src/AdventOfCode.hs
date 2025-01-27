module AdventOfCode (solutions, animations) where

import qualified Data.Map.Strict as M

import qualified Year2024.Solutions

solutions :: M.Map (Int, Int, Int) (String -> String)
solutions = Year2024.Solutions.solutions

animations :: M.Map (Int, Int, Int) (String -> [String])
animations = Year2024.Solutions.animations
