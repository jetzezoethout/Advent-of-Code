module HikingElement where

import           Direction (Direction (..))

data HikingElement
  = Path
  | Forest
  | Slope Direction
  deriving (Eq, Show)

fromChar :: Char -> HikingElement
fromChar '.' = Path
fromChar '#' = Forest
fromChar '^' = Slope North
fromChar '>' = Slope East
fromChar 'v' = Slope South
fromChar '<' = Slope West
fromChar _   = error "not a hiking element"

fromCharNonSlippery :: Char -> HikingElement
fromCharNonSlippery '#' = Forest
fromCharNonSlippery _   = Path
