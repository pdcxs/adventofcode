module Year2022.Day07 (solution1, solution2) where

data PathTree
  = Dir Int String [PathTree]
  | File Int

type Path = [String]

getSize :: PathTree -> Int
getSize (Dir s _ _) = s
getSize (File s) = s

appendTree ::
  PathTree ->
  [PathTree] ->
  Path ->
  PathTree
appendTree (Dir size name _) itms [] =
  Dir size name itms
appendTree
  (Dir size name chlds)
  itms
  (s : ss) =
    Dir size name (go chlds)
   where
    go (c : cs) =
      case c of
        File _ -> c : go cs
        Dir _ n _ ->
          if n == s
            then
              appendTree c itms ss : cs
            else c : go cs
    go [] = undefined
appendTree (File _) _ _ = undefined

parseInput :: String -> PathTree
parseInput =
  updateSize
    . go (Dir 0 "/" []) []
    . tail
    . lines
 where
  go tree _ [] = tree
  go tree dirs (ln : lns)
    | ln == "$ ls" =
        let (cts, lns') =
              span ((/= '$') . head) lns
            itms = map parseLine cts
            tree' = appendTree tree itms dirs
         in go tree' dirs lns'
    | ln == "$ cd .." =
        go tree (init dirs) lns
    | head ln == '$' =
        let folder = last (words ln)
         in go tree (dirs ++ [folder]) lns
    | otherwise = tree
  parseLine ('d' : cs) =
    Dir 0 (last (words cs)) []
  parseLine cs =
    let size = read $ head $ words cs
     in File size
  updateSize f@(File _) = f
  updateSize (Dir _ n chlds) =
    let chlds' = map updateSize chlds
        size = sum $ map getSize chlds'
     in Dir size n chlds'

search :: PathTree -> Int
search (File _) = 0
search (Dir s _ chlds)
  | s < 100000 = s + s'
  | otherwise = s'
 where
  s' = sum (map search chlds)

solution1 :: String -> IO ()
solution1 = print . search . parseInput

search' :: Int -> Int -> PathTree -> Int
search' _ s (File _) = s
search' target s (Dir size _ chlds) =
  minimum (map (search' target s') chlds)
 where
  s' =
    if size > target
      then min s size
      else s

solution2 :: String -> IO ()
solution2 input =
  print $
    search' target (maxBound :: Int) tree
 where
  tree = parseInput input
  target = getSize tree - 40000000
