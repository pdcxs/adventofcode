module Year2023.Day19 (solution1, solution2) where

import Control.Applicative hiding ((<|>))
import Control.Monad (void)
import Data.Char (isLower)
import Data.Either (fromRight)
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.String

type Item = M.Map Char Int
type Constraint = M.Map Char (Int, Int)
type System = M.Map String [Instruct]

data Instruct
  = Accept
  | Reject
  | Name String
  | Greater Char Int Instruct
  | Less Char Int Instruct
  deriving (Show)

processInput :: String -> (System, [Item])
processInput s = (system, items)
 where
  ls = lines s
  lss = splitWhen null ls
  s1 = lss !! 0
  s2 = lss !! 1
  system =
    M.fromList
      ( map
          ( fromRight ("", [])
              . parse lineP ""
          )
          s1
      )
  items = map parseItem s2

parseItem :: String -> Item
parseItem s = M.fromList itms
 where
  s' = init $ drop 1 s -- remove {}
  ss = splitOn "," s'
  itms =
    map
      ( \itm ->
          (itm !! 0, read (drop 2 itm))
      )
      ss

lineP :: Parser (String, [Instruct])
lineP = do
  name <- some letter
  void $ char '{'
  insts <- sepBy1 instP (char ',')
  void $ char '}'
  return (name, insts)

instP :: Parser Instruct
instP =
  try gtP
    <|> try ltP
    <|> try nameP
    <|> try acP
    <|> rjP

gtP :: Parser Instruct
gtP = do
  name <- oneOf "xmas"
  void $ char '>'
  num <- read <$> some digit
  void $ char ':'
  inst <- instP
  return (Greater name num inst)

ltP :: Parser Instruct
ltP = do
  name <- oneOf "xmas"
  void $ char '<'
  num <- read <$> some digit
  void $ char ':'
  inst <- instP
  return (Less name num inst)

nameP :: Parser Instruct
nameP = Name <$> some (satisfy isLower)

acP :: Parser Instruct
acP = Accept <$ char 'A'

rjP :: Parser Instruct
rjP = Reject <$ char 'R'

run :: System -> String -> Item -> Bool
run system name itm =
  case step (system M.! name) itm of
    Name s -> run system s itm
    Accept -> True
    Reject -> False
    _ -> undefined

step :: [Instruct] -> Item -> Instruct
step (Greater c n inst : insts) itm =
  if itm M.! c > n
    then inst
    else
      step insts itm
step (Less c n inst : insts) itm =
  if itm M.! c < n
    then inst
    else
      step insts itm
step (i : _) _ = i
step [] _ = undefined

solution1 :: String -> String
solution1 s =
  show $
    sum $
      map sum $
        map M.elems $
          filter (run system "in") itms
 where
  (system, itms) = processInput s

step' ::
  System ->
  [Instruct] ->
  Constraint ->
  [Constraint]
step' sys ((Greater c n inst) : insts) constraint =
  step'
    sys
    [inst]
    (merge (c, (n + 1, 4000)) constraint)
    ++ step'
      sys
      insts
      (merge (c, (1, n)) constraint)
step' sys ((Less c n inst) : insts) constraint =
  step'
    sys
    [inst]
    (merge (c, (1, n - 1)) constraint)
    ++ step'
      sys
      insts
      (merge (c, (n, 4000)) constraint)
step' _ (Accept : _) constraint = [constraint]
step' _ (Reject : _) _ = []
step' sys ((Name n) : _) c = step' sys (sys M.! n) c
step' _ [] _ = []

merge ::
  (Char, (Int, Int)) ->
  Constraint ->
  Constraint
merge (c, (low, high)) ct =
  M.insert c (low', high') ct
 where
  (l, h) = ct M.! c
  low' = max l low
  high' = min high h

getAnswer :: Constraint -> Int
getAnswer c =
  product $
    map (\(a, b) -> b - a + 1) $
      M.elems c

solution2 :: String -> String
solution2 s =
  show $
    sum $
      map getAnswer $
        step' system (system M.! "in") cs
 where
  (system, _) = processInput s
  cs = M.fromList $ zip "xmas" (repeat (1, 4000))
