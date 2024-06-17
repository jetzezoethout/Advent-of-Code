module SeaCucumber where

import           Direction (Direction (..))

data SeaCucumber
  = MovingEast
  | MovingSouth
  deriving (Eq, Show)

directionOf :: SeaCucumber -> Direction
directionOf MovingEast  = East
directionOf MovingSouth = South

fromChar :: Char -> Maybe SeaCucumber
fromChar '>' = Just MovingEast
fromChar 'v' = Just MovingSouth
fromChar '.' = Nothing
fromChar _   = error "unknown seafloor element"
