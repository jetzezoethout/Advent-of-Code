module Pipe where

import           Data.Maybe (fromMaybe)
import           Direction  (Direction (..), invert)

data Pipe
  = Connector Direction Direction
  | Start
  | Ground
  deriving (Eq, Show)

fromChar :: Char -> Pipe
fromChar char =
  case char of
    '|' -> Connector North South
    '-' -> Connector East West
    'L' -> Connector North East
    'J' -> Connector North West
    '7' -> Connector South West
    'F' -> Connector South East
    '.' -> Ground
    'S' -> Start
    _   -> error "not a pipe"

continue :: Direction -> Pipe -> Maybe Direction
continue movingTowards (Connector dir1 dir2)
  | movingTowards == invert dir1 = Just dir2
  | movingTowards == invert dir2 = Just dir1
  | otherwise = Nothing
continue _ _ = Nothing

unsafeContinue :: Direction -> Pipe -> Direction
unsafeContinue movingTowards pipe =
  fromMaybe (error "You died") $ continue movingTowards pipe
