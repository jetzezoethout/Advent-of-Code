module NonLocal where

import           Coordinate (Coordinate (..), addCoordinate)
import qualified Data.Map   as M
import           Seat       (Seat (..), WaitingArea, neighbourDeltas)

data Dimensions = Dimensions
  { height :: Int
  , width  :: Int
  }

isWithin :: Coordinate -> Dimensions -> Bool
Coordinate {..} `isWithin` Dimensions {..} =
  0 <= row && row < height && 0 <= column && column < width

isOccupiedTowards ::
     Dimensions -> WaitingArea -> Coordinate -> Coordinate -> Bool
isOccupiedTowards dimensions waitingArea location delta =
  go (addCoordinate location delta)
  where
    go probe =
      probe `isWithin` dimensions
        && (case M.lookup probe waitingArea of
              Just Occupied -> True
              Just Empty    -> False
              Nothing       -> go $ addCoordinate probe delta)

nonLocalSeatRound :: Dimensions -> WaitingArea -> WaitingArea
nonLocalSeatRound dimensions waitingArea =
  M.foldrWithKey' updateSeat waitingArea waitingArea
  where
    updateSeat :: Coordinate -> Seat -> WaitingArea -> WaitingArea
    updateSeat location Empty acc =
      if any (isOccupiedTowards dimensions waitingArea location) neighbourDeltas
        then acc
        else M.insert location Occupied acc
    updateSeat location Occupied acc =
      if length
           (filter
              (isOccupiedTowards dimensions waitingArea location)
              neighbourDeltas)
           >= 5
        then M.insert location Empty acc
        else acc
