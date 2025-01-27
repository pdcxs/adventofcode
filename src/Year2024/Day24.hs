{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2024.Day24 (solution1, solution2) where

import Control.Monad.State (
  MonadState (get),
  State,
  execState,
  modify,
 )
import Data.Bits (shiftL, xor, (.&.), (.|.))
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn, splitOneOf, splitWhen)
import qualified Data.Map.Strict as M

data Expr
  = Num Int
  | Var String
  | And Expr Expr
  | Or Expr Expr
  | Xor Expr Expr
  deriving (Show, Eq)

type Frame = M.Map String Expr

type Env a = State Frame a

processInput :: String -> Frame
processInput s = frame
 where
  [sec1, sec2] = splitWhen null $ lines s
  frame1 = go1 M.empty sec1
  go1 f [] = f
  go1 f (l : ls) = go1 (M.insert n v f) ls
   where
    [n, vs] = splitOn ": " l
    v = Num $ read vs
  frame = go2 frame1 sec2
  go2 f [] = f
  go2 f (l : ls) = go2 (M.insert n expr f) ls
   where
    [n1, op, n2, n] =
      filter (not . null) $
        splitOneOf " ->" l
    expr = case op of
      "AND" -> And (Var n1) (Var n2)
      "OR" -> Or (Var n1) (Var n2)
      "XOR" -> Xor (Var n1) (Var n2)
      _ -> undefined

eval :: String -> Env ()
eval v = do
  f <- get
  let expr = f M.! v
  case expr of
    Num _ -> return ()
    Var n -> eval n
    And (Var n1) (Var n2) -> do
      eval n1
      eval n2
      r <- applyFunc (.&.) n1 n2
      modify (M.insert v r)
    Or (Var n1) (Var n2) -> do
      eval n1
      eval n2
      r <- applyFunc (.|.) n1 n2
      modify (M.insert v r)
    Xor (Var n1) (Var n2) -> do
      eval n1
      eval n2
      r <- applyFunc xor n1 n2
      modify (M.insert v r)
    _ -> undefined

applyFunc ::
  (Int -> Int -> Int) ->
  String ->
  String ->
  Env Expr
applyFunc op n1 n2 = do
  f <- get
  let Num v1 = f M.! n1
      Num v2 = f M.! n2
  return $ Num $ op v1 v2

fromBinary :: [Expr] -> Int
fromBinary =
  sum
    . zipWith
      (*)
      (map (1 `shiftL`) [0 ..])
    . map toInt
 where
  toInt (Num n) = n
  toInt _ = undefined

run :: Frame -> Frame
run = execState m
 where
  m = do
    f <- get
    let vars = M.keys f
    mapM_ eval vars

solution1 :: String -> String
solution1 s =
  show . fromBinary $
    map (frame' M.!) zs
 where
  frame = processInput s
  frame' = run frame
  zs = sort $ filter ((== 'z') . head) $ M.keys frame'

-- a full adder gate:
-- https://en.wikipedia.org/wiki/Adder_(electronics)
-- input: xn yn cin
-- output: zn cout
-- a <- xn xor yn
-- b <- xn and yn
-- c <- cin and a
-- zn <- a xor cin
-- cout <- b or c
--
-- This function is large and ugly, I'm not satisfied.
-- I'm not sure whether all conditions are considered.
-- I just tried two inputs and both of them work.
-- If you find any problem, please contact me on github
getFullAdder ::
  Int ->
  String ->
  [(String, Expr)] ->
  (String, [String])
getFullAdder n cin f =
  ( cout'
  , aerr1
      ++ aerr2
      ++ cerr
      ++ zerr
      ++ couterr
      ++ berr
  )
 where
  d = if n >= 10 then show n else '0' : show n
  xn = 'x' : d
  yn = 'y' : d
  zn = 'z' : d

  a = fst . head $ filter (matchXor xn yn) f
  xorToCin =
    if n > 0
      then
        map (extract cin . snd) $
          filter (containsXor cin) f
      else []
  -- exchange a to b or c when produce zn
  aerr1 =
    if n > 0 && not (null xorToCin) && a /= head xorToCin
      then a : xorToCin
      else []
  -- exchange a to b when produce c
  aerr2 = [a | n > 0, not (any (matchAnd a cin) f)]

  -- whether z is error
  z =
    if n > 0
      then case filter (containsXor cin) f of
        ((v, _) : _) -> v
        _ -> cin
      else a
  zerr = [z | z /= zn]

  b = fst . head $ filter (matchAnd xn yn) f
  berr = [zn | b == zn] -- changed with z
  (c, cerr')
    | n == 0 = ("", [])
    | null aerr2 =
        (fst . head $ filter (matchAnd a cin) f, [])
    | otherwise =
        (fst . head $ filter (containsAnd cin) f, [b])
  cerr = if c == zn then zn : cerr' else cerr'
  cout
    | n == 0 = b
    | null aerr2 =
        if null berr
          then
            fst . head $ filter (containsOr b) f
          else
            fst . head $ filter (containsOr c) f
    | otherwise = fst . head $ filter (containsOr a) f
  couterr = [cout | cout == zn]
  cout' = if cout == zn then z else cout

containsOr :: String -> (String, Expr) -> Bool
containsOr n (_, Or (Var v1) (Var v2)) =
  v1 == n || v2 == n
containsOr _ _ = False

containsAnd :: String -> (String, Expr) -> Bool
containsAnd n (_, And (Var v1) (Var v2)) =
  v1 == n || v2 == n
containsAnd _ _ = False

matchAnd :: String -> String -> (String, Expr) -> Bool
matchAnd x y g = containsAnd x g && containsAnd y g

matchXor :: String -> String -> (String, Expr) -> Bool
matchXor x y g = containsXor x g && containsXor y g

extract :: String -> Expr -> String
extract s (Xor (Var n1) (Var n2)) =
  if n1 == s then n2 else n1
extract _ _ = undefined

containsXor :: String -> (String, Expr) -> Bool
containsXor n (_, Xor (Var v1) (Var v2)) =
  v1 == n || v2 == n
containsXor _ _ = False

solution2 :: String -> String
solution2 s =
  intercalate ","
    . sort
    . nubOrd
    $ go 0 ""
 where
  frame = M.assocs (processInput s)
  n = length (head $ splitWhen null $ lines s) `div` 2
  go i cin =
    if i < n
      then
        let (cout, err) = getFullAdder i cin frame
         in err ++ go (i + 1) cout
      else []
