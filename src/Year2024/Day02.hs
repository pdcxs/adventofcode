module Year2024.Day02 (solution1, solution2) where

processInput :: String -> [[Int]]
processInput = map (map read . words) . lines

isSafeOp :: (Int -> Int -> Bool) -> [Int] -> Bool
isSafeOp _ [] = True
isSafeOp _ [_] = True
isSafeOp op (x : y : xs)
 | op x y && d >= 1 && d <= 3 = isSafeOp op (y : xs)
 | otherwise = False
 where
  d = abs (x - y)

isSafe :: [Int] -> Bool
isSafe xs = isSafeOp (>) xs || isSafeOp (<) xs

solution1 :: String -> String
solution1 s = show $ length $ filter isSafe inputs
 where
  inputs = processInput s

rmIndex :: [Int] -> Int -> [Int]
rmIndex xs i = take i xs ++ drop (i + 1) xs

solution2 :: String -> String
solution2 s =
 show $
  length $
   filter isRmSafe inputs
 where
  inputs = processInput s

isRmSafe :: [Int] -> Bool
isRmSafe xs =
 isSafe xs
  || any
   (isSafe . rmIndex xs)
   [0 .. length xs - 1]
