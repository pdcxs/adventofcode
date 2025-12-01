module Year2025.Day01 (solution1, solution2) where

processInput :: String -> [Int]
processInput = map getNum . lines
 where
  getNum ('R' : xs) = read xs
  getNum ('L' : xs) = (-1) * read xs
  getNum xs = error ("getNum: " ++ xs)

scan :: Int -> Int -> [Int] -> Int
scan cnt _ [] = cnt
scan cnt n (x : xs) =
  let n' = (n + x) `mod` 100
      cnt' = if n' == 0 then cnt + 1 else cnt
   in scan cnt' n' xs

solution1 :: String -> IO ()
solution1 = print . scan 0 50 . processInput

scan' :: Int -> Int -> [Int] -> Int
scan' cnt _ [] = cnt
scan' cnt n (x : xs)
  | n'' == 0 = scan' (xr + 1) n'' xs
  | n == 0 = scan' xr n'' xs
  | n' < 0 = scan' (r + 1) n'' xs
  | n' > 99 = scan' r n'' xs
  | otherwise = scan' cnt n'' xs
 where
  n' = n + x
  n'' = n' `mod` 100
  r = cnt + (abs n' `div` 100)
  xr = cnt + (abs x `div` 100)

solution2 :: String -> IO ()
solution2 = print . scan' 0 50 . processInput
