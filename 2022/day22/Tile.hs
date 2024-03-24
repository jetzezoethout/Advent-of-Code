module Tile where

data Tile
  = Open
  | Wall
  | Void
  deriving (Show, Eq)

fromChar :: Char -> Tile
fromChar '.' = Open
fromChar '#' = Wall
fromChar ' ' = Void
fromChar _   = error "invalid tile"
