module Year2023.Day09 (solution1, solution2) where

processInput :: String -> [[Int]]
processInput = map (map read . words) . lines

step :: [Int] -> [Int]
step [] = [0]
step [x] = [x, x]
step xs
 | all (== 0) xs = 0 : xs
 | otherwise =
   (head xs - head next)
    : xs
    ++ [last xs + last next]
 where
  next = step $ zipWith subtract xs (tail xs)

getSum :: ([Int] -> Int) -> String -> Int
getSum pick = sum . map (pick . step) . processInput

solution1 :: String -> IO ()
solution1 = print . getSum last

solution2 :: String -> IO ()
solution2 = print . getSum head
