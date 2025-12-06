module Year2025.Day06 (solution1, solution2) where

import Data.List (transpose)

type Func = Int -> Int -> Int

processInput :: String -> ([Func], [[Int]])
processInput s =
  let ls = lines s
   in ( map parseFunc (words (last ls))
      , transpose $ map (map read . words) (init ls)
      )

parseFunc :: String -> Func
parseFunc "*" = (*)
parseFunc "+" = (+)
parseFunc f = error ("Unknown func: " ++ f)

solution1 :: String -> IO ()
solution1 =
  print
    . sum
    . uncurry (zipWith foldr1)
    . processInput

processInput' :: [String] -> ([Func], [[Int]])
processInput' ([] : _) = ([], [])
processInput' ls = (f : f', ns : ns')
 where
  lastLine = last ls
  len = length (takeWhile (== ' ') (tail lastLine))
  ns = map read $ transpose $ map (take len) (init ls)
  f = parseFunc (take 1 lastLine)
  ls' = map (drop (len + 1)) ls
  (f', ns') = processInput' ls'

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . uncurry (zipWith foldr1)
    . processInput'
    . map (++ " ")
    . lines
