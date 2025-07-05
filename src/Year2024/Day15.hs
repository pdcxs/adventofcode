module Year2024.Day15 (
 solution1,
 solution2,
 animation1,
 animation2,
) where

import Data.Char (isSpace)
import Data.List (partition)
import Data.Maybe (isNothing)

type Pos = (Int, Int)
type Dir = (Int, Int)

-- Map is set of boxes
-- a box contains some positions
type Box = [[Pos]]

type Wall = [Pos]

processInput ::
 ([String] -> [String]) ->
 String ->
 (Box, Wall, Pos, [Dir])
processInput f s =
 ( box
 , wall
 , p
 , map
    getDir
    ( filter (not . isSpace) $
       unlines insts
    )
 )
 where
  ls = lines s
  (graph, insts) = break null ls
  (box, wall, p) =
   parseMap
    0
    0
    []
    []
    []
    (f graph)

getDir :: Char -> Dir
getDir '>' = (1, 0)
getDir 'v' = (0, 1)
getDir '<' = (-1, 0)
getDir '^' = (0, -1)
getDir _ = undefined

parseMap ::
 Int -> -- current x
 Int -> -- current y
 Box -> -- current boxes
 Wall -> -- current walls
 [Pos] -> -- start location
 [String] -> -- input string
 (Box, Wall, Pos)
parseMap x y boxes walls startPos graph
 | null graph = (boxes, walls, head startPos)
 | null row = parseMap 0 (y + 1) boxes walls startPos rows
 | c == '.' = parseMap (x + 1) y boxes walls startPos next
 | c == 'O' = parseMap (x + 1) y boxes' walls startPos next
 -- for question 2
 | c == '[' = parseMap (x + 2) y boxes'' walls startPos next'
 | c == '@' = parseMap (x + 1) y boxes walls [(x, y)] next
 | c == '#' = parseMap (x + 1) y boxes walls' startPos next
 | otherwise = error ("Unrecognized char: " ++ [c])
 where
  row = head graph
  rows = tail graph
  c = head row
  cs = tail row
  next = cs : rows
  next' = tail cs : rows
  boxes' = [(x, y)] : boxes
  boxes'' = [(x, y), (x + 1, y)] : boxes
  walls' = (x, y) : walls

move :: Box -> Wall -> Pos -> Dir -> (Box, Pos)
move boxes walls (x, y) (dx, dy) =
 case pushBox boxes walls (dx, dy) (x, y) of
  Just boxes' -> (boxes', (x + dx, y + dy))
  Nothing -> (boxes, (x, y))

pushBox :: Box -> Wall -> Dir -> Pos -> Maybe Box
pushBox boxes walls dir@(dx, dy) (x, y)
 | nextPos `elem` walls = Nothing
 | null moved = Just boxes
 | isNothing boxes' = Nothing
 | otherwise = (moved' ++) <$> boxes'
 where
  nextPos = (x + dx, y + dy)
  (moved, unmoved) = partition (elem nextPos) boxes
  boxes' = go unmoved (concat moved) -- nextPushPos
  go bs [] = Just bs
  go bs (p : ps) =
   case pushBox bs walls dir p of
    Nothing -> Nothing
    Just bs' -> go bs' ps
  moved' =
   map
    ( map
       (\(bx, by) -> (bx + dx, by + dy))
    )
    moved

run :: Box -> Wall -> Pos -> [Dir] -> Int
run boxes _ _ [] =
 sum $
  map
   ((\(x, y) -> 100 * y + x) . last)
   boxes
run boxes walls pos (d : ds) =
 let (boxes', pos') = move boxes walls pos d
  in run boxes' walls pos' ds

solution1 :: String -> String
solution1 s = show $ run box wall start insts
 where
  (box, wall, start, insts) = processInput id s

solution2 :: String -> String
solution2 s = show $ run box wall start insts
 where
  (box, wall, start, insts) = processInput modify s
  modify = map (tail . concatMap expand)
  expand 'O' = "[]"
  expand '.' = ".."
  expand '@' = "@."
  expand '#' = "##"
  expand _ = undefined

-- We have already finished
-- below is for animation and debug
printMap :: Box -> Wall -> Pos -> Char -> String
printMap boxes walls pos c = unlines $ map (prefix . map locChar) locs
 where
  expanded = any ((> 1) . length) boxes
  prefix = if expanded then ('#' :) else id
  height = maximum (map snd walls)
  width = maximum (map fst walls)
  locs = [[(x, y) | x <- [0 .. width]] | y <- [0 .. height]]
  locChar p
   | p == pos = c
   | any (\b -> head b == p && length b == 1) boxes = 'O'
   | any (\b -> head b == p && length b == 2) boxes = '['
   | any (\b -> last b == p && length b == 2) boxes = ']'
   | p `elem` walls = '#'
   | otherwise = '.'

animation1 :: String -> [String]
animation1 s = runAnim box wall start insts
 where
  (box, wall, start, insts) = processInput id s

runAnim :: Box -> Wall -> Pos -> [Dir] -> [String]
runAnim boxes walls pos [] =
 [printMap boxes walls pos '@']
runAnim boxes walls pos (d : ds) =
 let (boxes', pos') = move boxes walls pos d
  in printMap boxes walls pos (showDir d)
      : runAnim boxes' walls pos' ds
 where
  showDir (-1, 0) = '<'
  showDir (1, 0) = '>'
  showDir (0, 1) = 'v'
  showDir (0, -1) = '^'
  showDir _ = undefined

animation2 :: String -> [String]
animation2 s = runAnim box wall start insts
 where
  (box, wall, start, insts) = processInput modify s
  modify = map (tail . concatMap expand)
  expand 'O' = "[]"
  expand '.' = ".."
  expand '@' = "@."
  expand '#' = "##"
  expand _ = undefined
