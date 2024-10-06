module PlaneSeat where

import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T

data PlaneSeatDirection
  = LowerHalf
  | UpperHalf

fromChar :: Char -> PlaneSeatDirection
fromChar 'F' = LowerHalf
fromChar 'B' = UpperHalf
fromChar 'L' = LowerHalf
fromChar 'R' = UpperHalf
fromChar _   = error "unknown direction to plane seat"

toBit :: PlaneSeatDirection -> Int
toBit LowerHalf = 0
toBit UpperHalf = 1

newtype PlaneSeat = PlaneSeat
  { directions :: [PlaneSeatDirection]
  }

parsePlaneSeat :: Text -> PlaneSeat
parsePlaneSeat = PlaneSeat . map fromChar . T.unpack

seatId :: PlaneSeat -> Int
seatId = foldl' (\acc bit -> 2 * acc + bit) 0 . map toBit . directions
