module Year2024.Day16 (
  solution1,
  solution2,
) where

import Common.Utils (safeHead)
import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S

type Pos = (Int, Int)
type Dir = (Int, Int)
type State = (Dir, Pos)

-- candidate search states
-- current score, list of: current state, prev state
type Candidate = M.Map Score [(State, [State])]
type Score = Int

-- state, score, prev states
type Record = M.Map State (Score, [State])

processInput :: String -> (Pos, Pos, S.Set Pos)
processInput = processMap 0 0 ([], [], S.empty)

processMap ::
  Int ->
  Int ->
  ([Pos], [Pos], S.Set Pos) ->
  String ->
  (Pos, Pos, S.Set Pos)
processMap _ _ (s, e, ps) [] = (safeHead s, safeHead e, ps)
processMap _ y (s, e, ps) ('\n' : ss) =
  processMap 0 (y + 1) (s, e, ps) ss
processMap x y (s, e, ps) ('.' : ss) =
  processMap (x + 1) y (s, e, S.insert (x, y) ps) ss
processMap x y (_, e, ps) ('S' : ss) =
  let loc = (x, y)
   in processMap (x + 1) y ([loc], e, S.insert loc ps) ss
processMap x y (s, _, ps) ('E' : ss) =
  let loc = (x, y)
   in processMap (x + 1) y (s, [loc], S.insert loc ps) ss
processMap x y (s, e, ps) (_ : ss) =
  processMap (x + 1) y (s, e, ps) ss

search :: S.Set Pos -> Candidate -> Record -> Record
search ps candidate rcd
  | M.null candidate = rcd
  | otherwise = search ps candidate'' rcd'
 where
  ((score, (st, prv)), candidate') = getNext candidate
  candidate'' = M.unionWith (++) (nextStates ps score st rcd) candidate'
  rcd' = updateRecord score st prv rcd

getNext :: Candidate -> ((Score, (State, [State])), Candidate)
getNext candidate = ((sc, (cur, prvs)), candidate')
 where
  (sc, states) = M.findMin candidate
  (cur, prvs) = safeHead states
  states' = drop 1 states
  candidate' =
    if null states'
      then
        M.delete sc candidate
      else M.insert sc states' candidate

nextStates :: S.Set Pos -> Score -> State -> Record -> Candidate
nextStates ps score st@((dx, dy), (x, y)) rcd =
  M.fromListWith
    (++)
    [ (score', [(((dx', dy'), (x', y')), [st])])
    | (score', (dx', dy'), (x', y')) <-
        [ (score + 1, (dx, dy), (x + dx, y + dy))
        , (score + 1000, (dy, -dx), (x, y))
        , (score + 1000, (-dy, dx), (x, y))
        ]
    , (x', y') `S.member` ps
    , notInOrBetter score ((dx', dy'), (x', y')) rcd
    ]

notInOrBetter :: Score -> State -> Record -> Bool
notInOrBetter score st rcd =
  case M.lookup st rcd of
    Nothing -> True
    Just (s, _) -> score <= s

updateRecord :: Score -> State -> [State] -> Record -> Record
updateRecord score st prev rcd =
  case M.lookup st rcd of
    Nothing -> M.insert st (score, prev) rcd
    Just (s, prevs) ->
      if score <= s
        then M.insert st (score, prev ++ prevs) rcd
        else rcd

allDirs :: [Dir]
allDirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

findMinScore :: Pos -> Record -> Int
findMinScore end rcd = minimum $ do
  dir <- allDirs
  let r = rcd M.!? (dir, end)
  guard (isJust r)
  return $ fst $ fromJust r

findPositionCount :: Pos -> Record -> Int
findPositionCount end rcd =
  S.size $
    S.map snd $
      findPath (zip allDirs (repeat end)) rcd S.empty

findPath :: [State] -> Record -> S.Set State -> S.Set State
findPath [] _ sts = sts
findPath (x : xs) rcd sts =
  case rcd M.!? x of
    Nothing -> findPath xs rcd sts
    Just (_, prevs) ->
      findPath
        ( filter
            (`S.notMember` sts)
            prevs
            ++ xs
        )
        rcd
        (S.insert x sts)

getRecord :: String -> (Pos, Record)
getRecord s = (end, search ps initialCandidate M.empty)
 where
  (start, end, ps) = processInput s
  initialCandidate = M.fromList [(0, [(((1, 0), start), [])])]

solution1 :: String -> String
solution1 = show . uncurry findMinScore . getRecord

solution2 :: String -> String
solution2 = show . uncurry findPositionCount . getRecord
