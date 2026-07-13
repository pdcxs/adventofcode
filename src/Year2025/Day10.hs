module Year2025.Day10 (solution1, solution2) where

import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)

type Bin = Int -- zero or one
type Value = Int
type Press = [Bin] -- Press or not for each button
type Button = [Bin] -- Corresponding lights connect or not
type Problem = ([Bin], [Button], [Value])

processInput :: String -> [Problem]
processInput = map parse . lines
 where
  parse s =
    let xs = words s
        n = length (head xs) - 2
     in ( map
            (\c -> if c == '#' then 1 else 0)
            (init . drop 1 $ head xs)
        , map (decode n . getNums) (init $ drop 1 xs)
        , getNums (last xs)
        )
  getNums = map read . splitOn "," . init . drop 1

decode :: Int -> [Int] -> Button
decode len btns = go [] (len - 1) (reverse btns)
 where
  go rs (-1) _ = rs
  go rs n [] = replicate (n + 1) 0 ++ rs
  go rs n xs@(i : is)
    | n == i = go (1 : rs) (n - 1) is
    | otherwise = go (0 : rs) (n - 1) xs

allCombs :: Int -> [Press]
allCombs = go [[]]
 where
  go xs 0 = xs
  go xs n = go (map (0 :) xs ++ map (1 :) xs) (n - 1)

allSelects :: Problem -> [Press]
allSelects (_, btns, _) = allCombs (length btns)

pressResult :: Problem -> Press -> [Value]
pressResult (_, btns, _) pressedBtns =
  foldr1 (zipWith (+)) $
    zipWith
      (\btn isPressed -> map (* isPressed) btn)
      btns
      pressedBtns

matchState :: Problem -> [Bin] -> Bool
matchState prob@(states, _, _) press =
  states == map (`mod` 2) (pressResult prob press)

getPressTimes :: Press -> Int
getPressTimes = sum

-- each button can only be pressed at most one time.
solver1 :: Problem -> Int
solver1 prob = minimum $ map getPressTimes candidates
 where
  allPress = allSelects prob
  candidates = filter (matchState prob) allPress

solution1 :: String -> IO ()
solution1 = print . sum . map solver1 . processInput

solver2 :: Problem -> Int
solver2 prob@(_, _, targets) = bitSearchMemoized targets 1
 where
  cands = allSelects prob
  prs = map (pressResult prob) cands
  bitSearchMemoized = memo2 bitSearch
  bitSearch tgts n
    | all (== 0) tgts = 0
    | null nexts || null result = minBound :: Int
    | otherwise = minimum result
   where
    n' = 2 * n
    tgts' =
      map (\pr -> zipWith subtract (map (* n) pr) tgts) prs
    validTarget tg = all (\t -> t `mod` n' == 0) tg && all (>= 0) tg
    nexts = filter (validTarget . fst) $ zip tgts' cands
    nextRs (tgs, press) =
      (getPressTimes press * n) + bitSearchMemoized tgs n'
    result = filter (>= 0) $ map nextRs nexts

solution2 :: String -> IO ()
solution2 = print . sum . map solver2 . processInput
