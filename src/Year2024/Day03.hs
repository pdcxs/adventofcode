module Year2024.Day03 (solution1, solution2) where

import Control.Applicative (
 Alternative (empty, many, (<|>)),
 )
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Maybe (fromJust)

newtype Parser a = Parser
 { runParser :: String -> Maybe (a, String)
 }

instance Functor Parser where
 fmap f (Parser p) = Parser $ \input ->
  case p input of
   Just (x, input') -> Just (f x, input')
   Nothing -> Nothing

instance Applicative Parser where
 pure x = Parser $ \input -> Just (x, input)
 (Parser p1) <*> (Parser p2) = Parser $ \input ->
  case p1 input of
   Just (f, input') ->
    case p2 input' of
     Just (x, input'') -> Just (f x, input'')
     Nothing -> Nothing
   Nothing -> Nothing

instance Monad Parser where
 (Parser p) >>= f = Parser $ \input ->
  case p input of
   Just (x, input') ->
    runParser (f x) input'
   Nothing -> Nothing

instance Alternative Parser where
 empty = Parser $ const Nothing
 (Parser p1) <|> (Parser p2) = Parser $ \input ->
  case p1 input of
   Just x -> Just x
   Nothing -> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
 (c : cs) ->
  if f c then Just (c, cs) else Nothing
 [] -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

anyChar :: Parser Char
anyChar = satisfy (const True)

data Inst
 = Mul Int Int
 | Do
 | DoNot
 | None
 deriving (Show)

mulParser :: Parser Inst
mulParser = do
 void $ string "mul("
 x <- read <$> many (satisfy isDigit)
 void $ char ','
 y <- read <$> many (satisfy isDigit)
 void $ char ')'
 return (Mul x y)

instParser :: Parser Inst
instParser = mulParser <|> (anyChar $> None)

solution1 :: String -> String
solution1 =
 show
  . eval True
  . fst
  . fromJust
  . runParser (many instParser)

solution2 :: String -> String
solution2 =
 show
  . eval True
  . fst
  . fromJust
  . runParser (many instParser')

eval :: Bool -> [Inst] -> Int
eval _ [] = 0
eval _ (Do : is) = eval True is
eval _ (DoNot : is) = eval False is
eval c (Mul x y : is) =
 if c
  then x * y + eval c is
  else eval c is
eval c (None : is) = eval c is

instParser' :: Parser Inst
instParser' =
 mulParser
  <|> (string "don't()" $> DoNot)
  <|> (string "do()" $> Do)
  <|> (anyChar $> None)
