module Year2022.Day06 (solution1, solution2) where

import qualified Data.Set as S

getMarker :: Int -> Int -> String -> Int
getMarker cnt len str =
 if S.size (S.fromList prefix) == len
  then cnt
  else getMarker (cnt + 1) len (tail str)
 where
  prefix = take len str

solution :: Int -> String -> IO ()
solution len input = do
 let answers =
      map
       (getMarker len len)
       (lines input)
 case answers of
  [r] -> print r
  _ -> print answers

solution1 :: String -> IO ()
solution1 = solution 4

solution2 :: String -> IO ()
solution2 = solution 14