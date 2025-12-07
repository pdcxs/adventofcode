module Year2025.Day07 (solution1, solution2) where

import Data.Containers.ListUtils (nubOrd)
import qualified Data.IntMap.Strict as M
import Data.MemoTrie (memo)
import qualified Data.Set as S

type Pos = (Int, Int)

-- key is x, value is set of y
type Map = M.IntMap (S.Set Int)

processInput :: String -> (Map, [Pos])
processInput input = (mp, [pos])
 where
  (pos, mp) = parseMap 0 0 [] M.empty (lines input)
  parseMap _ _ s m [] = (head s, m)
  parseMap _ y s m ([] : ss) =
    parseMap 0 (y + 1) s m ss
  parseMap x y _ m (('S' : cs) : ss) =
    parseMap (x + 1) y [(x, y)] m (cs : ss)
  parseMap x y s m (('^' : cs) : ss) =
    parseMap
      (x + 1)
      y
      s
      (M.insertWith S.union x (S.singleton y) m)
      (cs : ss)
  parseMap x y s m ((_ : cs) : ss) =
    parseMap (x + 1) y s m (cs : ss)

split :: Map -> Pos -> ([Pos], [Pos])
split m (x, y) =
  case (m M.!? x) >>= S.lookupGT y of
    Nothing -> ([], [(x, y)])
    Just sy -> ([(x, sy)], [(x - 1, sy), (x + 1, sy)])

step ::
  Map -> [Pos] -> (S.Set Pos, [Pos])
step m ls = (sp, ls')
 where
  sps = map (split m) ls
  ls' = nubOrd (concatMap snd sps)
  sp = S.fromList (concatMap fst sps)

getSplitters ::
  S.Set Pos -> Map -> [Pos] -> S.Set Pos
getSplitters sps m ls
  | S.null sps' = sps
  | otherwise =
      getSplitters (S.union sps sps') m ls'
 where
  (sps', ls') = step m ls

solution1 :: String -> IO ()
solution1 =
  print
    . S.size
    . uncurry (getSplitters S.empty)
    . processInput

solution2 :: String -> IO ()
solution2 s = print (count (head start) :: Int)
 where
  (m, start) = processInput s
  count ls
    | null sps = 1
    | otherwise = sum $ map (memo count) ls'
   where
    (sps, ls') = split m ls
