module Year2022.Day02 (solution1, solution2) where

data Choice = Rock | Paper | Scissors
 deriving (Eq)

parseInput :: String -> [(Choice, Choice)]
parseInput = map parseLine . lines
 where
  parseLine ln =
   ( \xs ->
      ( toC1 (head xs)
      , toC2 (last xs)
      )
   )
    $ words ln
  toC1 "A" = Rock
  toC1 "B" = Paper
  toC1 "C" = Scissors
  toC1 _ = undefined
  toC2 "X" = Rock
  toC2 "Y" = Paper
  toC2 "Z" = Scissors
  toC2 _ = undefined

getScore :: (Choice, Choice) -> Int
getScore (c1, c2) = choiceScore c2 + outcomeScore (c1, c2)
 where
  choiceScore Rock = 1
  choiceScore Paper = 2
  choiceScore Scissors = 3
  outcomeScore (Rock, Rock) = 3
  outcomeScore (Rock, Paper) = 6
  outcomeScore (Rock, Scissors) = 0
  outcomeScore (Paper, Rock) = 0
  outcomeScore (Paper, Paper) = 3
  outcomeScore (Paper, Scissors) = 6
  outcomeScore (Scissors, Rock) = 6
  outcomeScore (Scissors, Paper) = 0
  outcomeScore (Scissors, Scissors) = 3

solution1 :: String -> IO ()
solution1 =
 print
  . sum
  . map getScore
  . parseInput

getChoice :: (Choice, Choice) -> (Choice, Choice)
getChoice (c1, c2) = (c1, getC c2)
 where
  getC :: Choice -> Choice
  getC Rock -- corresponding to X
   | c1 == Rock = Scissors
   | c1 == Paper = Rock
   | otherwise = Paper
  -- corresponding to Y
  getC Paper = c1
  getC Scissors -- corresponding to Z
   | c1 == Rock = Paper
   | c1 == Paper = Scissors
   | otherwise = Rock

solution2 :: String -> IO ()
solution2 =
 print
  . sum
  . map (getScore . getChoice)
  . parseInput
