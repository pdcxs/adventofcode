module Year2024.Day16 (
  solution1,
  solution2,
  animation1,
) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (find, foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S

type Pos = (Int, Int)

type Dir = (Int, Int)

type State = (Pos, Dir)

type Score = Int

type Wall = S.Set Pos

-- a node is (score, prev states, cur state)
type Node = (Score, [State], State)

-- Current State, Current Score, Prev State
type Record = M.Map State (Score, [State])

processInput :: String -> (Pos, Pos, Wall)
processInput = parseMap 0 0 S.empty [] []

parseMap ::
  Int -> -- currentX
  Int -> -- currentY
  Wall -> -- Wall positions
  [Pos] -> -- start position
  [Pos] -> -- end position
  String -> -- input string
  (Pos, Pos, Wall) -- start end walls
parseMap _ _ walls s e [] = (head s, head e, walls)
parseMap _ y walls s e ('\n' : cs) =
  parseMap 0 (y + 1) walls s e cs
parseMap x y walls _ e ('S' : cs) =
  parseMap (x + 1) y walls [(x, y)] e cs
parseMap x y walls s _ ('E' : cs) =
  parseMap (x + 1) y walls s [(x, y)] cs
parseMap x y walls s e ('#' : cs) =
  parseMap (x + 1) y (S.insert (x, y) walls) s e cs
parseMap x y walls s e (_ : cs) =
  parseMap (x + 1) y walls s e cs

search ::
  Wall -> -- walls
  Pos -> -- end position
  Record -> -- recorded states and scores
  -- candidates (score, prev state, cur state)
  [Node] ->
  Record -- final records
search walls end record candidates
  | null candidates = record -- this should not be happend
  | fst nextState == end = record' -- finish
  | otherwise =
      -- trace (show candidates) $
      search walls end record' candidates'
 where
  ((nextScore, nextPrev, nextState), css) = getMin candidates
  record' = M.insertWith merge nextState (nextScore, nextPrev) record
  merge (oldScore, oldPrevs) (newScore, newPrevs)
    | oldScore < newScore = (oldScore, oldPrevs)
    | oldScore > newScore = (newScore, newPrevs)
    | otherwise = (oldScore, nubOrd $ newPrevs ++ oldPrevs)
  cs =
    filter
      ( \(sc, _, st@(p, _)) ->
          p `S.notMember` walls
            && better st sc
      )
      $ nextCandidates nextScore nextState
  candidates' = mergeSts cs css
  better st sc = case (M.!?) record st of
    Nothing -> True
    Just (sc', _) -> sc <= sc'

nextCandidates :: Score -> State -> [Node]
nextCandidates score st@(pos@(x, y), dir@(dx, dy)) =
  [ (score + 1, [st], ((x + dx, y + dy), dir))
  , (score + 1000, [st], (pos, (-dy, dx)))
  , (score + 1000, [st], (pos, (dy, -dx)))
  ]

getMin :: [Node] -> (Node, [Node])
getMin (x : xs) = foldl' go (x, []) xs
 where
  go (minX@(minScore, _, stx), others) y@(ySc, _, sty)
    | ySc <= minScore = (y, minX : others)
    | stx == sty = (minX, others)
    | otherwise = (minX, y : others)
getMin [] = undefined

solution1 :: String -> String
solution1 s = show . fst $ (M.!) records st
 where
  (start, end, walls) = processInput s
  initState = (start, (1, 0))
  initCandidates = nextCandidates 0 initState
  initRecord = M.singleton initState (0, [])
  records = search walls end initRecord initCandidates
  st = fromJust $ find ((== end) . fst) $ M.keys records

solution2 :: String -> String
solution2 s = show . length . nubOrd $ go st
 where
  (start, end, walls) = processInput s
  initState = (start, (1, 0))
  initCandidates = nextCandidates 0 initState
  initRecord = M.singleton initState (0, [])
  records = search walls end initRecord initCandidates
  st = fromJust $ find ((== end) . fst) $ M.keys records
  go k = case (M.!) records k of
    (_, []) -> []
    (_, ps) -> fst k : concatMap go ps

searchAnim ::
  Wall -> -- walls
  Pos -> -- end position
  Record -> -- recorded states and scores
  -- candidates (score, prev state, cur state)
  [Node] ->
  [String]
searchAnim walls end record candidates
  | null candidates = [currentMap] -- this should not be happend
  | fst nextState == end = [currentMap]
  | otherwise = currentMap : searchAnim walls end record' candidates'
 where
  ((nextScore, nextPrev, nextState), css) = getMin candidates
  record' = M.insertWith merge nextState (nextScore, nextPrev) record
  merge (oldScore, oldPrevs) (newScore, newPrevs)
    | oldScore < newScore = (oldScore, oldPrevs)
    | oldScore > newScore = (newScore, newPrevs)
    | otherwise = (oldScore, nubOrd $ newPrevs ++ oldPrevs)
  cs =
    filter
      ( \(sc, _, st@(p, _)) ->
          p `S.notMember` walls
            && better st sc
      )
      $ nextCandidates nextScore nextState
  candidates' = mergeSts cs css
  better st sc = case (M.!?) record st of
    Nothing -> True
    Just (sc', _) -> sc <= sc'
  currentMap = printMap walls end record candidates

mergeSts :: [Node] -> [Node] -> [Node]
mergeSts [] ys = ys
mergeSts (x@(scx, prevX, stx) : xs) ys =
  case findSt stx ys of
    Nothing -> x : mergeSts xs ys
    Just ((sc, prevY, _), ys') ->
      if scx < sc
        then x : mergeSts xs ys'
        else
          if scx > sc
            then mergeSts xs ys
            else
              (sc, nubOrd (prevX ++ prevY), stx)
                : mergeSts xs ys'

findSt ::
  State ->
  [(Score, [State], State)] ->
  Maybe
    ( (Score, [State], State)
    , [(Score, [State], State)]
    )
findSt _ [] = Nothing
findSt stx (y@(_, _, sty) : ys) =
  if stx == sty
    then Just (y, ys)
    else case findSt stx ys of
      Nothing -> Nothing
      Just (y', ys') -> Just (y', y : ys')

printMap ::
  Wall -> -- walls
  Pos -> -- end pos
  Record -> -- Current Records
  [Node] ->
  String
printMap walls end record candidates =
  go 0 0
 where
  height = maximum $ S.map snd walls
  width = maximum $ S.map fst walls
  go x y
    | x > width = '\n' : go 0 (y + 1)
    | (x, y) `S.member` walls = '#' : go (x + 1) y
    | (x, y) == nextPos = dirChar nextDir : go (x + 1) y
    | inCandidates && inRecord = 'X' : go (x + 1) y
    | inCandidates = '?' : go (x + 1) y
    | inRecord = 'O' : go (x + 1) y
    | (x, y) == end = 'E' : go (x + 1) y
    | y > height = []
    | otherwise = '.' : go (x + 1) y
   where
    inCandidates =
      any
        (\(_, _, (p, _)) -> p == (x, y))
        candidates
    inRecord = any ((== (x, y)) . fst) (M.keys record)
    (next, _) = getMin candidates
    (nextPos, nextDir) = third next
    third (_, _, v) = v
    dirChar (1, 0) = '>'
    dirChar (-1, 0) = '<'
    dirChar (0, 1) = 'v'
    dirChar (0, -1) = '^'
    dirChar _ = undefined

animation1 :: String -> [String]
animation1 s = searchAnim walls end initRecord initCandidates
 where
  (start, end, walls) = processInput s
  initState = (start, (1, 0))
  initCandidates = nextCandidates 0 initState
  initRecord = M.singleton initState (0, [])
