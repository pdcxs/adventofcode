module Year2022.Day10 (solution1, solution2) where

data Op = Noop | Inc Int

processInput :: String -> [Op]
processInput = map parse . lines
 where
  parse "noop" = Noop
  parse s = Inc (read $ dropWhile (/= ' ') s)

step :: Int -> Op -> [Int]
step x Noop = [x]
step x (Inc v) = [x + v, x]

run :: [Op] -> [Int]
run = reverse . go [1]
 where
  go vs [] = vs
  go xs@(v : _) (o : os) = go (step v o ++ xs) os
  go [] _ = undefined

getResult :: [Int] -> [Int] -> Int
getResult = go 1
 where
  go _ [] _ = 0
  go t (d : ds) (v : vs)
    | t == d = t * v + go (t + 1) ds vs
    | otherwise = go (t + 1) (d : ds) vs
  go _ _ _ = undefined

solution1 :: String -> IO ()
solution1 =
  print
    . getResult [20, 60 .. 220]
    . run
    . processInput

generateLine :: [Int] -> (String, [Int])
generateLine vs = (r, vs')
 where
  (xs, vs') = splitAt 40 vs
  r = zipWith f xs [0 .. 39]
  f x y = if abs (x - y) < 2 then '#' else '.'

getScreen :: [Int] -> String
getScreen vs =
  let (r, vs') = generateLine vs
   in if null vs' || length vs' < 40
        then r
        else r ++ "\n" ++ getScreen vs'

solution2 :: String -> IO ()
solution2 = putStrLn . getScreen . run . processInput
