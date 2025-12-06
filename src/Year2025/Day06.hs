module Year2025.Day06 (solution1, solution2) where

type Func = Int -> Int -> Int

processInput :: String -> ([Func], [[Int]])
processInput s = (getFunc cols, getNums cols)
 where
  inputs = map words $ lines s
  cols = getCol inputs
  getCol ([] : _) = []
  getCol xs = map head xs : getCol (map tail xs)
  getNums = map (map read . init)
  getFunc =
    map
      ( parseFunc
          . last
      )

parseFunc :: String -> Func
parseFunc "*" = (*)
parseFunc "+" = (+)
parseFunc s = error $ "ParseFunc: unknown " ++ s

solution1 :: String -> IO ()
solution1 =
  print
    . sum
    . uncurry (zipWith foldr1)
    . processInput

-- last line indicates num length
getCols :: [String] -> ([Func], [[Int]])
getCols ([] : _) = ([], [])
getCols ls =
  let ns = map (take len) (init ls)
      ls' = map (drop (len + 1)) ls
      (fs, xs) = getCols ls'
   in (f : fs, getNum ns : xs)
 where
  lastLine = last ls
  -- last column length should be fixed
  fix = if all (== ' ') (tail lastLine) then 1 else 0
  len =
    (+ fix) . length $
      takeWhile (== ' ') (tail lastLine)
  f = parseFunc (take 1 lastLine)
  getNum :: [String] -> [Int]
  getNum ([] : _) = []
  getNum xs =
    read (map head xs)
      : getNum (map tail xs)

solution2 :: String -> IO ()
solution2 =
  print
    . sum
    . uncurry (zipWith foldr1)
    . getCols
    . lines
