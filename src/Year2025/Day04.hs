module Year2025.Day04 (solution1, solution2) where

import qualified Data.Set as S

type Pos = (Int, Int)
type Map = S.Set Pos

processInput :: String -> (Int, Int, Map)
processInput s = (h, w, mp)
 where
  ls = lines s
  mp = parseMap 0 0 S.empty ls
  h = length ls
  w = length (head ls)
  parseMap _ _ m [] = m
  parseMap _ y m ("" : ss) = parseMap 0 (y + 1) m ss
  parseMap x y m (('@' : cs) : ss) =
    parseMap (x + 1) y (S.insert (x, y) m) (cs : ss)
  parseMap x y m ((_ : cs) : ss) =
    parseMap (x + 1) y m (cs : ss)

getNeighbors :: (Int, Int, Map) -> Pos -> [Pos]
getNeighbors (h, w, m) (x, y) =
  [ (nx, ny)
  | nx <- [x - 1 .. x + 1]
  , nx >= 0
  , nx < w
  , ny <- [y - 1 .. y + 1]
  , ny >= 0
  , ny < h
  , (nx, ny) /= (x, y)
  , (nx, ny) `S.member` m
  ]

getPos :: (Int, Int, Map) -> [Pos]
getPos (h, w, m) = rs
 where
  pos = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
  rs =
    filter
      ( \p ->
          p `S.member` m
            && length (getNeighbors (h, w, m) p) < 4
      )
      pos

solution1 :: String -> IO ()
solution1 = print . length . getPos . processInput

repeatGetPos :: (Int, Int, Map) -> Int
repeatGetPos (h, w, m)
  | null next = 0
  | otherwise = n + repeatGetPos (h, w, m')
 where
  next = getPos (h, w, m)
  n = length next
  m' = foldr S.delete m next

solution2 :: String -> IO ()
solution2 = print . repeatGetPos . processInput
