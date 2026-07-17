-- Use Map and Set to make this more clear.
-- But performance is a little bit worse than list version
module Year2025.Day10V2 (solution1, solution2) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List.Split (splitOn)
import Data.MemoTrie (memo2)
import qualified Data.Set as S

type Value = Int -- value of lights
type Bin = Int -- zero or one
type LightState = IS.IntSet -- lights to be turned on
type LightValue = IM.IntMap Value -- key is LightId
type LightTarget = [Value] -- key is LightId
type Button = IM.IntMap Bin -- Key is LightId
type Problem = (LightState, S.Set Button, LightTarget)
type Press = S.Set Button -- which button should be pressed

processInput :: String -> [Problem]
processInput = map parse . lines
 where
  rmEnds = init . drop 1
  getState =
    IS.fromAscList
      . map fst
      . filter ((== '#') . snd)
      . zip [0 ..]
      . rmEnds
  getNums = map read . splitOn "," . rmEnds
  getButtons = IM.fromAscList . flip zip (repeat 1) . getNums
  getTargets = getNums
  parse ln = (state, btns, targets)
   where
    ws = words ln
    state = getState (head ws)
    targets = getTargets (last ws)
    btns = S.fromList $ map getButtons (rmEnds ws)

allPresses :: Problem -> S.Set Press
allPresses (_, btns, _) = S.powerSet btns

getPressResult :: Press -> LightValue
getPressResult = S.foldr (IM.unionWith (+)) IM.empty

isStateMatch :: Problem -> LightValue -> Bool
isStateMatch (st, _, _) lv = IM.keysSet lv == st

solver1 :: Problem -> Int
solver1 p =
  minimum
    . S.map snd
    . S.filter (isStateMatch p . fst)
    . S.map
      (\pr -> (IM.filter odd $ getPressResult pr, S.size pr))
    $ allPresses p

solution1 :: String -> IO ()
solution1 = print . sum . map solver1 . processInput

solver2 :: Problem -> Int
solver2 p@(_, _, tgts) = memoSearch 1 tgts
 where
  apList =
    map (\pr -> (getPressResult pr, S.size pr)) $
      S.toList (allPresses p)
  memoSearch = memo2 search
  search n tg
    | all (== 0) tg = 0
    | null rs = minBound :: Int
    | otherwise = minimum rs
   where
    n' = n * 2
    tgMap = IM.fromAscList (zip [0 ..] tg)
    getNewTg prMap =
      IM.mapWithKey
        (\k v -> v - n * IM.findWithDefault 0 k prMap)
        tgMap
    nexts =
      filter
        ( \(tgts', _) ->
            all (>= 0) tgts' && all (\t -> t `mod` n' == 0) tgts'
        )
        $ map
          (\(prMap, pn) -> (IM.elems (getNewTg prMap), pn))
          apList
    getNext (tgts', pn) = n * pn + memoSearch n' tgts'
    rs = filter (>= 0) $ map getNext nexts

solution2 :: String -> IO ()
solution2 = print . sum . map solver2 . processInput
