module Main (main) where

import AdventOfCode (animations, solutions)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  if length args < 3
    then do
      printHelp
    else do
      let (year, day, question) = getTimeQuestion args
          test = isTest args
          inputFile = getFile (year, day, test)
      contents <- readFile inputFile
      if question < 3
        then
          let func = solutions M.! (year, day, question)
           in putStrLn $ func contents
        else do
          let _func = animations M.! (year, day, question)
              _anims = _func contents -- generate animation texts
              _delay = getDelay args
           in putStrLn "not implement yet"

getTimeQuestion :: [String] -> (Int, Int, Int)
getTimeQuestion args = (year, day, question)
 where
  qs = map read $ take 3 args
  year = head qs
  day = qs !! 1
  question = last qs

getFile :: (Int, Int, Bool) -> FilePath
getFile (year, day, test) = "inputs" </> show year </> file
 where
  file = prefix ++ formatNum day ++ ".txt"
  prefix = if test then "test" else "input"

getDelay :: [String] -> Double
getDelay args =
  if length args > 4 && args !! 3 == "test"
    || length args > 3 && args !! 3 /= "test"
    then fromMaybe 1.0 (readMaybe (last args))
    else 1.0 :: Double

isTest :: [String] -> Bool
isTest args = length args > 3 && args !! 3 == "test"

formatNum :: Int -> String
formatNum i = if i < 10 then '0' : s else s
 where
  s = show i

printHelp :: IO ()
printHelp = do
  putStrLn $
    "usage: program_name"
      ++ " year day question [test]"
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 1 1\" will run first question"
      ++ " of day1, 2024 year."
  putStrLn $
    "For example: \"stack run --"
      ++ " 2024 1 2 test\" will run second"
      ++ " question of day1, 2024 year with test input."
