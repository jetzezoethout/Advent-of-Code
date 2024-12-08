module Jet where

import           Data.Text (Text)
import qualified Data.Text as T
import           Direction (Direction (East, West))

data Jet
  = L
  | R
  deriving (Show)

fromChar :: Char -> Jet
fromChar '<' = L
fromChar '>' = R
fromChar _   = error "not a jet"

parseJets :: Text -> [Jet]
parseJets = map fromChar . T.unpack . T.strip

toDirection :: Jet -> Direction
toDirection L = West
toDirection R = East
