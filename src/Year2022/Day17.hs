module Year2022.Day17 (solution1, solution2) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type Dir = Int
type Pos = (Int, Int) -- y and x
type Brick = S.Set Pos
type Locs = S.Set Pos
type State =
  (Bool, Int, Locs, [Brick], [Dir], [Int])

processInput :: String -> [Dir]
processInput =
  cycle
    . concatMap
      ( \case
          '<' -> [-1]
          '>' -> [1]
          _ -> []
      )

bricks :: [Brick]
bricks = cycle [b1, b2, b3, b4, b5]

b1 :: Brick
b1 = S.fromAscList (map (0,) [2 .. 5])

b2 :: Brick
b2 =
  S.fromAscList
    [(0, 3), (1, 2), (1, 3), (1, 4), (2, 3)]

b3 :: Brick
b3 =
  S.fromAscList
    [(0, 2), (0, 3), (0, 4), (1, 4), (2, 4)]

b4 :: Brick
b4 = S.fromAscList (map (,2) [0 .. 3])

b5 :: Brick
b5 = S.fromAscList [(0, 2), (0, 3), (1, 2), (1, 3)]

push :: Locs -> Brick -> Dir -> Maybe Brick
push locs brick dir =
  if isValidX locs brick'
    then Just brick'
    else Nothing
 where
  brick' = S.map (\(y, x) -> (y, x + dir)) brick

isValidX :: Locs -> Brick -> Bool
isValidX locs brick =
  minX >= 0 && maxX <= 6 && S.null inter
 where
  xs = S.map snd brick
  minX = S.findMin xs
  maxX = S.findMax xs
  inter = S.intersection brick locs

fall :: Locs -> Brick -> Maybe Brick
fall locs brick =
  if isValidY locs brick'
    then Just brick'
    else Nothing
 where
  brick' = S.map (\(y, x) -> (y - 1, x)) brick

isValidY :: Locs -> Brick -> Bool
isValidY locs brick = minY >= 0 && S.null inter
 where
  minY = fst $ S.findMin brick
  inter = S.intersection brick locs

step :: State -> State
step (isNew, count, locs, b : bs, d : ds, _) =
  let nb =
        if isNew
          then S.map (\(y, x) -> (y + maxY + 4, x)) b
          else b
      b' = fromMaybe nb (push locs nb d)
      b'' = fall locs b'
      locs' = S.union b' locs
      xs = S.toList $ S.map snd b'
      maxY =
        if S.null locs then -1 else fst $ S.findMax locs
   in case b'' of
        Just x -> (False, count, locs, x : bs, ds, [])
        Nothing ->
          (True, count + 1, locs', bs, ds, xs)
step _ = error "step error"

simulateTo :: Int -> [Dir] -> Locs
simulateTo n dirs = go (True, 0, S.empty, bricks, dirs, [])
 where
  go (t, c, locs, bs, ds, _)
    | c == n = locs
    | otherwise = go (step (t, c, locs, bs, ds, []))

getHeight :: Int -> [Dir] -> Int
getHeight n = (+ 1) . fst . S.findMax . simulateTo n

solution1 :: String -> IO ()
solution1 =
  print
    . getHeight 2022
    . processInput

-- get cycle start index and end index
getCycle :: [Dir] -> (Int, Int)
getCycle dirs = go M.empty rcd
 where
  rcd =
    filter (not . null . getXs) $
      iterate
        step
        (True, 0, S.empty, bricks, dirs, [])
  getXs (_, _, _, _, _, xs) = xs
  checkSize = 50
  go seen rs =
    let compared = map getXs $ take checkSize rs
        (_, cnt, _, _, _, _) = head rs
     in case M.lookup compared seen of
          Just i -> (i, cnt)
          Nothing -> go (M.insert compared cnt seen) (tail rs)

solution2 :: String -> IO ()
solution2 s = print $ numCycle * diffHeight + remainHeight
 where
  dirs = processInput s
  (start, end) = getCycle dirs
  startHeight = getHeight start dirs
  endHeight = getHeight end dirs
  diffHeight = endHeight - startHeight
  cycleLen = end - start
  numCycle = (n - start) `div` cycleLen
  remain = (n - start) `mod` cycleLen
  remainHeight = getHeight (remain + start) dirs
  n = 1000000000000
