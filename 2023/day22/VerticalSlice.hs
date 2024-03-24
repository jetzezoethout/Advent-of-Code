module VerticalSlice where

import           Brick           (highestZ)
import           Coordinate      (Coordinate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           SettledBrick    (BrickId, SettledBrick (..))

data VerticalSlice = VerticalSlice
  { highestBrickId   :: Maybe BrickId
  , highestOccupiedZ :: Int
  } deriving (Show)

emptySlice :: VerticalSlice
emptySlice = VerticalSlice Nothing 0

putBrick :: SettledBrick -> VerticalSlice
putBrick SettledBrick {..} =
  VerticalSlice
    {highestBrickId = Just brickId, highestOccupiedZ = highestZ brick}

type Towers = Map Coordinate VerticalSlice

getOrElseEmpty :: Coordinate -> Towers -> VerticalSlice
getOrElseEmpty coord = fromMaybe emptySlice . M.lookup coord
