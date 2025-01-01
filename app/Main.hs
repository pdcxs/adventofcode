module Main (main) where

import AdventOfCode (solutions)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  args <- getArgs
  if length args < 3
    then do
      putStrLn "usage: program_name year day question [test]"
      putStrLn "For example: \"stack run -- 2024 1 1\" will run first question of day1, 2024 year."
      putStrLn "For example: \"stack run -- 2024 1 2 test\" will run second question of day1, 2024 year with test input."
    else do
      let qs = map read $ take 3 args
          year = head qs
          day = qs !! 1
          question = last qs
          func = solutions M.! (year, day, question)
          file =
            if length args > 3
              then "test" ++ show day ++ ".txt"
              else "input" ++ show day ++ ".txt"
          filePath = "inputs" </> show year </> file
      contents <- readFile filePath
      putStrLn $ func contents
