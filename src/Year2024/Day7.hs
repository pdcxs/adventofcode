module Year2024.Day7 (solution1, solution2) where

processInput :: String -> [(Int, [Int])]
processInput = map processLine . lines
  where
    processLine l = (target l, remains l)
    target = read . takeWhile (/= ':')
    remains = map read . words . tail . dropWhile (/= ':')

type Op = Int -> Int -> Int

combs :: Int -> [Op] -> [Int] -> [Int]
combs _ _ [] = []
combs t _ [x] = [x | t == x]
combs t ops (x : y : xs) =
  concatMap
    (\n -> combs t ops (n : xs))
    nums
  where
    nums = filter (<= t) $ map (\f -> f x y) ops

isRight :: [Op] -> Int -> [Int] -> Bool
isRight ops target xs = target `elem` combs target ops xs

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