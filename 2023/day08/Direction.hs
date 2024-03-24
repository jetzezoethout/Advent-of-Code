module Direction where

import           Data.Text (Text)
import qualified Data.Text as T

data Direction
  = L
  | R
  deriving (Show)

fromChar :: Char -> Direction
fromChar 'L' = L
fromChar 'R' = R
fromChar _   = error "not a direction"

parseDirections :: Text -> [Direction]
parseDirections = map fromChar . T.unpack
