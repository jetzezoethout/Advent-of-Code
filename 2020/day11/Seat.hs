module Seat where

import           Coordinate  (Coordinate (..), addCoordinate)
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import           LocatedChar (LocatedChar (..), locateTextLines)

data Seat
  = Empty
  | Occupied
  deriving (Show, Eq)

type WaitingArea = Map Coordinate Seat

parseWaitingArea :: [Text] -> WaitingArea
parseWaitingArea = M.fromList . mapMaybe findEmptySeats . locateTextLines
  where
    findEmptySeats LocatedChar {..} = [(location, Empty) | char == 'L']

isOccupiedAt :: WaitingArea -> Coordinate -> Bool
waitingArea `isOccupiedAt` coord = M.lookup coord waitingArea == Just Occupied

occupiedSeats :: WaitingArea -> Int
occupiedSeats = length . filter (== Occupied) . M.elems

localSeatRound :: WaitingArea -> WaitingArea
localSeatRound waitingArea = M.foldrWithKey' updateSeat waitingArea waitingArea
  where
    updateSeat :: Coordinate -> Seat -> WaitingArea -> WaitingArea
    updateSeat location Empty acc =
      if any (waitingArea `isOccupiedAt`) (neighbours location)
        then acc
        else M.insert location Occupied acc
    updateSeat location Occupied acc =
      if length (filter (waitingArea `isOccupiedAt`) (neighbours location)) >= 4
        then M.insert location Empty acc
        else acc

neighbourDeltas :: [Coordinate]
neighbourDeltas =
  [ Coordinate row col
  | row <- [-1, 0, 1]
  , col <- [-1, 0, 1]
  , not (row == 0 && col == 0)
  ]

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (addCoordinate coord) neighbourDeltas
