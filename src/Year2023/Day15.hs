module Year2023.Day15 (solution1, solution2) where

import Control.Applicative (Alternative (some))
import Control.Monad (void)
import Data.Char (isSpace, ord)
import Data.Either (fromRight)
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String

type Hash = Int

processInput :: String -> [String]
processInput = splitOn "," . filter (not . isSpace)

getHash :: String -> Hash
getHash = foldl (\acc x -> (acc + ord x) * 17 `mod` 256) 0

solution1 :: String -> String
solution1 = show . sum . map getHash . processInput

data Item = Add String Int | Remove String
  deriving (Show)

itemP :: Parser Item
itemP = try addP <|> removeP

addP :: Parser Item
addP = do
  name <- some (noneOf "=-")
  void $ char '='
  len <- some digit
  return $ Add name (read len)

removeP :: Parser Item
removeP = do
  name <- some (noneOf "=-")
  void $ char '-'
  return $ Remove name

processInput' :: String -> [Item]
processInput' = fromRight [] . parse (sepBy1 itemP (char ',')) ""

type Boxes = V.Vector [(String, Int)]

process :: Item -> [(String, Int)] -> [(String, Int)]
process (Add name len) [] = [(name, len)]
process itm@(Add name len) ((n, l) : itms)
  | name == n = (n, len) : itms
  | otherwise = (n, l) : (process itm itms)
process (Remove _) [] = []
process itm@(Remove name) (i@(n, _) : itms)
  | name == n = itms
  | otherwise = i : process itm itms

initBoxes :: Boxes
initBoxes = V.replicate 256 []

solve :: Boxes -> [Item] -> Boxes
solve boxes [] = boxes
solve boxes (itm : itms) =
  let hash = getHash (getName itm)
      oldItms = boxes V.! hash
      boxes' = boxes V.// [(hash, process itm oldItms)]
   in solve boxes' itms

getName :: Item -> String
getName (Add n _) = n
getName (Remove n) = n

getAnswer :: Boxes -> Int
getAnswer =
  fst
    . V.foldl'
      ( \(r, idx) itms ->
          ( r
              + idx
                * ( sum
                      ( zipWith
                          (*)
                          (map snd itms)
                          [1 ..]
                      )
                  )
          , idx + 1
          )
      )
      (0, 1)

solution2 :: String -> String
solution2 = show . getAnswer . solve initBoxes . processInput'
