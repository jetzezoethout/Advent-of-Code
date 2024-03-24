module GroundElement where

import           MirrorEq (MirrorEq (..), MirrorEquality (..))

data GroundElement
  = Ash
  | Rock
  deriving (Eq, Show)

fromChar :: Char -> GroundElement
fromChar '.' = Ash
fromChar '#' = Rock
fromChar _   = error "not a ground element"

instance MirrorEq GroundElement where
  mirrorEquals :: GroundElement -> GroundElement -> MirrorEquality
  Ash `mirrorEquals` Ash   = Equal
  Rock `mirrorEquals` Rock = Equal
  _ `mirrorEquals` _       = OffByOne
