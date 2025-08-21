module Year2023.Day20 (
 solution0, -- plot circuit
 solution1,
 solution2,
) where

import Control.Monad (guard)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Text.Printf (printf)

type Pulse = Bool
type Name = String
type Memory = M.Map Name Pulse

data Module
 = FlipFlop Pulse [Name]
 | Conjunction Memory [Name]
 | Broadcast [Name]
 deriving (Show)

type Circuit = M.Map Name Module

-- from, to, pulse
type Action = (Name, Name, Pulse)

processInput :: String -> Circuit
processInput input =
 fixCirc $
  M.fromList $
   map parse ls
 where
  ls = lines input
  parse ('b' : xs) = ("broadcaster", Broadcast (getNames xs))
  parse ('%' : xs) = (getName xs, getFlp xs)
  parse ('&' : xs) = (getName xs, getCon xs)
  parse _ = undefined
  getFlp xs = FlipFlop False (getNames xs)
  getCon xs =
   let names = getNames xs
    in Conjunction M.empty names

getName :: [Char] -> [Char]
getName xs = takeWhile (/= ' ') xs

getNames :: [Char] -> [[Char]]
getNames xs =
 map (filter (/= ',')) $
  words $
   tail $
    dropWhile (/= '>') xs

fixCirc :: Circuit -> Circuit
fixCirc c = go (M.toList c) c
 where
  go [] c' = c'
  go ((_, Conjunction _ _) : cs) c' = go cs c'
  go ((n, m) : cs) c' =
   go cs (insertMem n (getNexts m) c')
  insertMem _ [] c' = c'
  insertMem n (m : ms) c' = case M.lookup m c' of
   Just (Conjunction mem outs) ->
    let mem' = M.insert n False mem
        c'' = M.insert m (Conjunction mem' outs) c'
     in insertMem n ms c''
   _ -> insertMem n ms c'

getNexts :: Module -> [Name]
getNexts (FlipFlop _ outs) = outs
getNexts (Conjunction _ outs) = outs
getNexts (Broadcast outs) = outs

pushButton ::
 Circuit ->
 [Action] ->
 Int ->
 Int ->
 (Int, Int, Circuit)
pushButton c [] h l = (h, l, c)
pushButton c ((f, t, p) : as) h l = case m of
 Nothing -> pushButton c as h l
 Just (FlipFlop state outs) ->
  if p
   then pushButton c as h l
   else
    let c' = M.insert t (FlipFlop (not state) outs) c
        sig = not state
        as' = map (t,,sig) outs
        h' = if sig then h + length outs else h
        l' = if sig then l else l + length outs
     in pushButton c' (as ++ as') h' l'
 Just (Conjunction mem outs) ->
  let mem' = M.insert f p mem
      sig = not $ all id (M.elems mem')
      c' = M.insert t (Conjunction mem' outs) c
      h' = if sig then h + length outs else h
      l' = if sig then l else l + length outs
      as' = map (t,,sig) outs
   in pushButton c' (as ++ as') h' l'
 Just (Broadcast outs) ->
  pushButton
   c
   (as ++ map (t,,p) outs)
   h
   (1 + l + length outs) -- button counts 1
 where
  m = M.lookup t c
countSig :: Circuit -> Int -> Int
countSig = go 0 0
 where
  go h l _ 0 = h * l
  go h l c n =
   let (h', l', c') = pushButton c start h l
    in go h' l' c' (n - 1)

start :: [Action]
start = [("", "broadcaster", False)]

solution1 :: String -> IO ()
solution1 input = print (countSig c 1000)
 where
  c = processInput input

-- There are some sub-circuits
-- we need the output of all sub-circuits
-- to be high pulse
-- we can run `stack run 2023 20 0`
-- to get plot code
-- put the generated code to
-- https://mermaid.live/edit
-- to see the graph
solution2 :: String -> IO ()
solution2 input =
 print $
  foldr1 lcm $
   M.elems
    (go c records (1 :: Integer))
 where
  c = processInput input
  prevs = do
   (n, m) <- M.toList c
   let outs = getNexts m
   let r = M.lookup (head outs) c
   guard (isJust r)
   let m' = fromJust r
   let outs' = getNexts m'
   guard (outs' == ["rx"])
   return n
  records = M.fromList (zip prevs (repeat 0))
  go circ rcd n
   | all (> 0) (M.elems rcd) = rcd
   | otherwise =
     let (circ', activated) =
          pushButton' circ prevs [] start
         rcd' = updateRecords rcd activated n
      in go circ' rcd' (n + 1)
  updateRecords rcd [] _ = rcd
  updateRecords rcd (m : ms) n =
   case M.lookup m rcd of
    Just x ->
     if x > 0
      then updateRecords rcd ms n
      else
       updateRecords (M.insert m n rcd) ms n
    _ -> updateRecords rcd ms n

pushButton' ::
 Circuit ->
 [Name] ->
 [Name] ->
 [Action] ->
 (Circuit, [Name])
pushButton' c _ activated [] = (c, activated)
pushButton' c names activated ((f, t, p) : as) =
 case m of
  Nothing -> pushButton' c names activated as
  Just (FlipFlop state outs) ->
   if p
    then pushButton' c names activated as
    else
     let c' =
          M.insert
           t
           (FlipFlop (not state) outs)
           c
         sig = not state
         as' = map (t,,sig) outs
         act =
          if sig
           then
            [ act'
            | act' <- outs
            , act' `elem` names
            ]
           else []
      in pushButton'
          c'
          names
          (act ++ activated)
          (as ++ as')
  Just (Conjunction mem outs) ->
   let mem' = M.insert f p mem
       sig = not $ all id (M.elems mem')
       c' = M.insert t (Conjunction mem' outs) c
       as' = map (t,,sig) outs
       act =
        if not sig
         then
          [act' | act' <- outs, act' `elem` names]
         else []
    in pushButton'
        c'
        names
        (act ++ activated)
        (as ++ as')
  Just (Broadcast outs) ->
   pushButton'
    c
    names
    []
    (as ++ map (t,,p) outs)
 where
  m = M.lookup t c

solution0 :: String -> IO ()
solution0 =
 putStrLn . unlines
  . ("flowchart TD" :)
  . map ("  " ++)
  . arrange [] [] []
  . concatMap parseLine
  . lines
 where
  parseLine ln@('b' : _) =
   map
    ("broadcaster --> " ++)
    (getNames ln)
  parseLine ('%' : xs) =
   let name = getName xs
       names = getNames xs
    in map (printf "%s --> %s" name) names
  parseLine ('&' : xs) =
   let name = getName xs
       names = getNames xs
    in map (printf "%s{%s} --> %s" name name) names
  parseLine _ = []
  arrange broads outputs others [] =
   broads ++ others ++ outputs
  arrange broads outputs others (l : ls)
   | take 11 l == "broadcaster" =
     arrange (l : broads) outputs others ls
   | drop (length l - 2) l == "rx" =
     arrange broads (outputs ++ [l]) others ls
   | '{' `elem` l =
     arrange broads (l : outputs) others ls
   | otherwise = arrange broads outputs (l : others) ls
