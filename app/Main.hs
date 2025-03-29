module Main (main) where

import AdventOfCode (animations, solutions)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Linear (V2 (V2))
import Raylib.Core
import Raylib.Core.Text
import Raylib.Util.Colors (black, white)
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
          let func = animations M.! (year, day, question)
              anims = func contents -- generate animation texts
              fps = getArgFPS args
           in animation anims fps

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

getArgFPS :: [String] -> Int
getArgFPS args =
  if length args > 4 && args !! 3 == "test"
    || length args > 3 && args !! 3 /= "test"
    then fromMaybe 1 (readMaybe (last args))
    else 1

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

fontSize :: Int
fontSize = 15

animation :: [String] -> Int -> IO ()
animation ss fps = do
  setTargetFPS fps
  let ls = lines (head ss)
      width = length (head ls)
      height = length ls
      w = fromIntegral (width * fontSize) * 0.54 :: Double
      h = fromIntegral (height * fontSize) * 1.13 :: Double
  win <- initWindow (ceiling w) (ceiling h) "Animation"
  font <- loadFont "assets/consola.ttf"
  loop win font (cycle ss)
 where
  loop winr ft (f : fs) = do
    shouldClose <- windowShouldClose
    if not shouldClose
      then do
        beginDrawing
        clearBackground white
        drawTextEx ft f (V2 0 0) (fromIntegral fontSize) 0.0 black
        endDrawing
        loop winr ft fs
      else do
        closeWindow (Just winr)
  loop _ _ [] = undefined
