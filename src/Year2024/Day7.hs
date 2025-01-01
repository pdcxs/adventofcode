module Year2024.Day7 (solution1, solution2) where

processInput :: String -> [(Int, [Int])]
processInput = map processLine . lines
  where
    processLine l = (target l, remains l)
    target = read . takeWhile (/= ':')
    remains = map read . words . tail . dropWhile (/= ':')

type Op = Int -> Int -> Int

combs :: [Op] -> [Int] -> [Int]
combs _ [] = []
combs _ [x] = [x]
combs ops (x : y : xs) =
  concatMap
    (\n -> combs ops (n : xs))
    nums
  where
    nums = map (\f -> f x y) ops

isRight :: [Op] -> Int -> [Int] -> Bool
isRight ops target xs = target `elem` combs ops xs

solution1 :: String -> String
solution1 =
  show
    . sum
    . map fst
    . filter (uncurry (isRight [(+), (*)]))
    . processInput

-- very slow, but it just works.
solution2 :: String -> String
solution2 =
  show
    . sum
    . map fst
    . filter (uncurry (isRight [(+), (*), conc]))
    . processInput
  where
    conc x y = read (show x ++ show y)