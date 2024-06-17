module BingoNumber where

data BingoNumber
  = Marked
      { value :: Int
      }
  | Unmarked
      { value :: Int
      }
  deriving (Show, Eq)

isMarked :: BingoNumber -> Bool
isMarked (Marked _)   = True
isMarked (Unmarked _) = False

mark :: Int -> BingoNumber -> BingoNumber
mark number onCard =
  if onCard == Unmarked number
    then Marked number
    else onCard
