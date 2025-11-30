module Year2022.Day16 (solution1, solution2) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

type Pos = String
type Flow = M.Map Pos Int
type Map = M.Map Pos [Pos]
type Graph = M.Map (Pos, Pos) Int -- distances
type Record = IM.IntMap [Pos]

processInput :: String -> (Flow, Graph)
processInput s = (flow, graph)
 where
  ls = lines s
  parseLine ln =
    let wds = words ln
        name = wds !! 1
        flw = init (drop 5 (wds !! 4))
        dirs = map (filter (/= ',')) $ drop 9 wds
     in ((name, read flw), (name, dirs))
  raw = map parseLine ls
  flow =
    M.fromList
      . filter (\(p, r) -> r > 0 || p == "AA")
      $ map fst raw
  m = M.fromList $ map snd raw
  graph = buildGraph (M.keys flow) m

buildGraph :: [Pos] -> Map -> Graph
buildGraph = go M.empty
 where
  go g [] _ = g
  go g (x : xs) m =
    let edgs = map (x,) xs
        dists =
          map
            ( \(s, e) ->
                search
                  m
                  e
                  IM.empty
                  (IM.singleton 0 [s])
            )
            edgs
        g' = M.fromList $ zip edgs dists
     in go (M.union g g') xs m

search :: Map -> Pos -> Record -> Record -> Int
search m end visited candidat
  | end == nextPos = nextDist
  | otherwise = search m end visited' candidat'
 where
  (next@(nextPos, nextDist), cand) = getBest candidat
  visited' = IM.insertWith (++) nextDist [nextPos] visited
  candidat' =
    IM.fromAscList
      . filter (not . null . snd)
      . IM.toAscList
      . IM.map
        (filter (`notElem` concat (IM.elems visited')))
      $ IM.unionWith
        (++)
        cand
        (getNeighbors m visited next)

getBest :: Record -> ((Pos, Int), Record)
getBest rcd =
  let ((d, ps), rcd') = IM.deleteFindMin rcd
   in case ps of
        [x] -> ((x, d), rcd')
        (x : xs) -> ((x, d), IM.insert d xs rcd')
        _ -> undefined

getNeighbors ::
  Map -> Record -> (Pos, Int) -> Record
getNeighbors m rcd (p, d) =
  IM.fromListWith
    (++)
    [ (d + 1, [nextPos])
    | nextPos <- M.findWithDefault [] p m
    , nextPos `notElem` concat (IM.elems rcd)
    ]

walk :: Graph -> Flow -> Pos -> Int -> Int
walk graph flow pos time
  | time <= 0 = 0
  | M.null flow = 0
  | null nexts = (time - 1) * rate
  | rate == 0 = maximum nextMoves
  | otherwise =
      (time - 1) * rate
        + maximum nexts
 where
  rate = flow M.! pos
  flow' = M.delete pos flow
  next =
    [ (np, nt)
    | np <- M.keys flow'
    , let nt = time - 1 - getDist pos np
    ]
  nextMv =
    [ (np, nt)
    | np <- M.keys flow'
    , let nt = time - getDist pos np
    ]
  getDist from to =
    case graph M.!? (from, to) of
      Just d -> d
      Nothing -> graph M.! (to, from)
  nexts = map (uncurry (walk graph flow')) next
  nextMoves = map (uncurry (walk graph flow')) nextMv

solution1 :: String -> IO ()
solution1 s = print $ walk graph flow "AA" 30
 where
  (flow, graph) = processInput s

splitFlow ::
  [(Pos, Int)] -> [([(Pos, Int)], [(Pos, Int)])]
splitFlow = go [([], [])]
 where
  go r f
    | null f = r
    | otherwise =
        let (pos, rate) = head f
            f' = tail f
         in go
              ( concatMap
                  ( \(left, right) ->
                      [ ((pos, rate) : left, right)
                      , (left, (pos, rate) : right)
                      ]
                  )
                  r
              )
              f'

getSplitResult ::
  Graph -> ([(Pos, Int)], [(Pos, Int)]) -> Int
getSplitResult graph (left, right) =
  ls + rs
 where
  getFlow = M.insert "AA" 0 . M.fromAscList . reverse
  ls = walk graph (getFlow left) "AA" 26
  rs = walk graph (getFlow right) "AA" 26

solution2 :: String -> IO ()
solution2 s =
  print $
    maximum (map (getSplitResult graph) splits)
 where
  (flow, graph) = processInput s
  splits = splitFlow (M.toAscList flow)
