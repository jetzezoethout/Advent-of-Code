module SnailfishNumber where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard)
import           Control.Monad.State (StateT (StateT), evalStateT)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read      (decimal)
import           Leaf                (Direction (..), Leaf (..), addToLeaf,
                                      depth, magnitudeContribution, nest,
                                      splitLeaf)

newtype SnailfishNumber = SnailfishNumber
  { getLeaves :: [Leaf]
  } deriving (Show)

type Parser = StateT Text Maybe

readChar :: Parser Char
readChar = StateT T.uncons

dropChar :: Char -> Parser ()
dropChar ch = do
  actual <- readChar
  guard $ actual == ch

readInt :: Parser Int
readInt =
  StateT $ \text ->
    case decimal text of
      Left _      -> Nothing
      Right value -> Just value

regularNumber :: [Direction] -> Parser [Leaf]
regularNumber path = do
  val <- readInt
  return [Leaf {value = val, path = path}]

pair :: [Direction] -> Parser [Leaf]
pair path = do
  dropChar '['
  leftPart <- snailfishNumber (L : path)
  dropChar ','
  rightPart <- snailfishNumber (R : path)
  dropChar ']'
  return $ leftPart <> rightPart

snailfishNumber :: [Direction] -> Parser [Leaf]
snailfishNumber path = pair path <|> regularNumber path

parseSnailfishNumber :: Text -> SnailfishNumber
parseSnailfishNumber =
  SnailfishNumber . fromJust . evalStateT (snailfishNumber [])

magnitude :: SnailfishNumber -> Int
magnitude SnailfishNumber {..} = sum $ map magnitudeContribution getLeaves

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
SnailfishNumber leftLeaves `add` SnailfishNumber rightLeaves =
  reduce $ SnailfishNumber $ map (nest L) leftLeaves <> map (nest R) rightLeaves

splitOnce :: [Leaf] -> Maybe [Leaf]
splitOnce = go
  where
    go [] = Nothing
    go (leaf:leaves) =
      if value leaf >= 10
        then Just (splitLeaf leaf <> leaves)
        else (leaf :) <$> go leaves

explodeOnce :: [Leaf] -> Maybe [Leaf]
explodeOnce = go Nothing
  where
    go :: Maybe Leaf -> [Leaf] -> Maybe [Leaf]
    go previous (first:second:rest) =
      if depth first >= 5
        then Just
               $ prependMaybe (addToLeaf (value first) <$> previous)
               $ Leaf {value = 0, path = tail (path first)}
                   : applyToHead (addToLeaf (value second)) rest
        else prependMaybe previous <$> go (Just first) (second : rest)
    go _ _ = Nothing
    prependMaybe :: Maybe a -> [a] -> [a]
    prependMaybe = maybe id (:)
    applyToHead :: (a -> a) -> [a] -> [a]
    applyToHead _ []     = []
    applyToHead f (x:xs) = f x : xs

reduceOnce :: [Leaf] -> Maybe [Leaf]
reduceOnce leaves =
  case explodeOnce leaves of
    Nothing  -> splitOnce leaves
    exploded -> exploded

reduce :: SnailfishNumber -> SnailfishNumber
reduce SnailfishNumber {..} = SnailfishNumber $ go getLeaves
  where
    go leaves = maybe leaves go (reduceOnce leaves)
