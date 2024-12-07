module SeaFloor where

import           Coordinate  (Coordinate (..))
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import           Direction   (Direction, moveTowards)
import           LocatedChar (LocatedChar (..), locateTextLines)
import           SeaCucumber (SeaCucumber (..), directionOf, fromChar)

data Dimensions = Dimensions
  { height :: Int
  , width  :: Int
  } deriving (Show)

moveOnSeaFloor :: Dimensions -> Coordinate -> Direction -> Coordinate
moveOnSeaFloor Dimensions {..} coord dir =
  fitOnSeaFloor $ coord `moveTowards` dir
  where
    fitOnSeaFloor Coordinate {..} =
      Coordinate {row = row `mod` height, column = column `mod` width}

type SeaFloor = Map Coordinate SeaCucumber

parseSeaFloor :: [Text] -> SeaFloor
parseSeaFloor = M.fromList . mapMaybe parseCucumber . locateTextLines
  where
    parseCucumber LocatedChar {..} = (location, ) <$> fromChar char

moveUntilStop :: Dimensions -> SeaFloor -> Int
moveUntilStop dimensions = go 0
  where
    go timesMoved seaFloor =
      let newFloor = moveCucumbers dimensions seaFloor
       in if seaFloor == newFloor
            then timesMoved + 1
            else go (timesMoved + 1) newFloor

moveCucumbers :: Dimensions -> SeaFloor -> SeaFloor
moveCucumbers dimensions =
  moveCucumberKind dimensions MovingSouth
    . moveCucumberKind dimensions MovingEast

moveCucumberKind :: Dimensions -> SeaCucumber -> SeaFloor -> SeaFloor
moveCucumberKind dimensions cucumberKind seaFloor =
  M.foldrWithKey' go seaFloor seaFloor
  where
    go :: Coordinate -> SeaCucumber -> SeaFloor -> SeaFloor
    go coord cucumber acc =
      let targetLocation =
            moveOnSeaFloor dimensions coord $ directionOf cucumber
       in if cucumberKind == cucumber && targetLocation `M.notMember` seaFloor
            then M.insert targetLocation cucumber $ M.delete coord acc
            else acc
