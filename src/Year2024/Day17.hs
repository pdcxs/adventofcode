module Year2024.Day17 (
  solution1,
  solution2,
  animation,
) where

import Control.Monad (when)
import Control.Monad.State (
  MonadState (get, put),
  State,
  execState,
 )
import Data.Bits (
  Bits (shiftR, xor, (.&.)),
 )
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Vector.Primitive as V

type Code = V.Vector Int

data Machine = Machine
  { regA :: Int
  , regB :: Int
  , regC :: Int
  , pointer :: Int
  , outputs :: [Int]
  }
  deriving (Show)

processInput :: String -> (Int, Code)
processInput s = (a, code)
 where
  ls = lines s
  regLs = take 3 ls
  a = read $ filter isDigit $ head regLs
  codeLs = dropWhile (not . isDigit) $ last ls
  code =
    V.fromList $
      map read (splitOn "," codeLs)

type Env = State Machine

type Operator = Int -> Env ()

next :: Env ()
next = do
  m <- get
  let p = pointer m
  put (m{pointer = p + 2})

combo :: Int -> Env Int
combo operand = do
  if operand < 4
    then
      return operand
    else do
      m <- get
      case operand of
        4 -> return $ regA m
        5 -> return $ regB m
        6 -> return $ regC m
        _ -> undefined

adv :: Operator
adv operand = do
  op <- combo operand
  m <- get
  let a = regA m
  put (m{regA = a `shiftR` op})

bxl :: Operator
bxl operand = do
  m <- get
  let b = regB m
  put (m{regB = b `xor` operand})

bst :: Operator
bst operand = do
  op <- combo operand
  m <- get
  put (m{regB = op .&. 7})

jnz :: Operator
jnz operand = do
  m <- get
  let a = regA m
  when (a /= 0) $
    put (m{pointer = operand - 2})

bxc :: Operator
bxc _ = do
  m <- get
  let b = regB m
      c = regC m
  put (m{regB = b `xor` c})

out :: Operator
out operand = do
  op <- combo operand
  m <- get
  let o = outputs m
  put (m{outputs = o ++ [op .&. 7]})

bdv :: Operator
bdv operand = do
  op <- combo operand
  m <- get
  let a = regA m
  put (m{regB = a `shiftR` op})

cdv :: Operator
cdv operand = do
  op <- combo operand
  m <- get
  let a = regA m
  put (m{regC = a `shiftR` op})

run :: Code -> Env ()
run code = do
  m <- get
  let p = pointer m
  when (p < V.length code) $ do
    let op = code V.! p
        operand = code V.! (p + 1)
    case op of
      0 -> adv operand
      1 -> bxl operand
      2 -> bst operand
      3 -> jnz operand
      4 -> bxc operand
      5 -> out operand
      6 -> bdv operand
      7 -> cdv operand
      _ -> undefined
    next
    run code

getResult :: Int -> Code -> [Int]
getResult a code =
  outputs $
    execState
      (run code)
      (Machine a 0 0 0 [])

solution1 :: String -> IO ()
solution1 =
  putStrLn
    . init
    . tail
    . show
    . uncurry getResult
    . processInput

-- The program has some pattern
-- convert number to 8-based
-- change highest digit only influence
-- lower digits of outputs
-- and outputs has same length of 8-based input
-- you can use following code to see this

-- solution2 :: String -> IO ()
-- solution2 s = putStrLn $ unlines (map (show . go) [1 .. 8 ^ 4])
--  where
--   (_, code) = processInput s
--   go v =
--     ( toBase8 v
--     , getResult v code
--     )
--   toBase8 v =
--     let (h, l) = v `quotRem` 8
--      in if h == 0 then l else 10 * toBase8 h + l

solution2 :: String -> IO ()
solution2 s =
  print (go minVal)
 where
  (_, code) = processInput s
  len = V.length code
  minVal = 8 ^ (len - 1)
  lstCode = V.toList code
  rcode = reverse lstCode
  go :: Int -> Int
  go v
    | l' < 0 = v
    | length outs /= len = undefined
    | otherwise = go (v + 8 ^ l')
   where
    outs = getResult v code
    l' =
      len
        - 1
        - getCommonLength rcode (reverse outs)

getCommonLength :: [Int] -> [Int] -> Int
getCommonLength [] _ = 0
getCommonLength _ [] = 0
getCommonLength (x : xs) (y : ys)
  | x == y = 1 + getCommonLength xs ys
  | otherwise = 0

animation :: String -> [String]
animation s = go (len - 1) minVal
 where
  (_, code) = processInput s
  len = V.length code
  minVal = 8 ^ (len - 1)
  prtCode = show lstCode
  lstCode = V.toList code
  rcode = reverse lstCode
  go :: Int -> Int -> [String]
  go l v
    | l' < 0 = repeat (present v l' outs)
    | length outs /= len = repeat prt
    | otherwise = prt : go l' (v + 8 ^ l')
   where
    outs = getResult v code
    l' =
      len
        - 1
        - getCommonLength rcode (reverse outs)
    prt = present v l outs
  present v l outs =
    unlines
      [ showBase8 v ++ ", loc = " ++ show l
      , show v
      , prtCode
      , show outs
      , show (drop l lstCode)
      , show (drop l outs)
      ]
  showBase8 v =
    let (h, l) = quotRem v 8
     in if h == 0
          then show l
          else
            showBase8 h ++ show l
