module HexDirection where

import           Control.Applicative (Alternative (many, (<|>)))
import           Control.Monad       (guard)
import           Control.Monad.State (StateT (StateT), evalStateT)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T

data HexDirection
  = East
  | NorthEast
  | NorthWest
  | West
  | SouthWest
  | SouthEast
  deriving (Show, Enum)

allHexDirections :: [HexDirection]
allHexDirections = [East .. SouthEast]

type Parser = StateT Text Maybe

readChar :: Parser Char
readChar = StateT T.uncons

dropChar :: Char -> Parser ()
dropChar ch = do
  actual <- readChar
  guard $ actual == ch

dropText :: Text -> Parser ()
dropText text =
  case T.uncons text of
    Nothing                   -> return ()
    Just (leading, remaining) -> dropChar leading >> dropText remaining

readHexDirection :: Parser HexDirection
readHexDirection =
  (dropText "e" >> return East)
    <|> (dropText "ne" >> return NorthEast)
    <|> (dropText "nw" >> return NorthWest)
    <|> (dropText "w" >> return West)
    <|> (dropText "sw" >> return SouthWest)
    <|> (dropText "se" >> return SouthEast)

parseHexDirections :: Text -> [HexDirection]
parseHexDirections = fromJust . evalStateT (many readHexDirection)
