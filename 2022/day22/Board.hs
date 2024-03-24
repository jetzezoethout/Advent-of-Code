module Board where

import           Coordinate (Coordinate (..))
import           Data.List
import           Data.Maybe (fromJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Direction  (Direction (..), moveTowards, moveTowardsBy)
import           Grid       (Grid (..), atCoordinate, fromNestedList,
                             safeAtCoordinate)
import           Tile       (Tile (..), fromChar)
import           WalkState  (WalkState (..))

data UnitSquare

type Board = Grid Tile

parseBoard :: [Text] -> Board
parseBoard textLines =
  fromNestedList
    $ map (map fromChar . T.unpack . T.justifyLeft width ' ') textLines
  where
    width = maximum $ map T.length textLines

sideLength :: Board -> Int
sideLength board = min board.height board.width `div` 3

isOnBoard :: Coordinate -> Board -> Bool
coord `isOnBoard` board = maybe False (/= Void) $ board `safeAtCoordinate` coord

shiftInside :: Board -> Coordinate -> Coordinate
shiftInside board Coordinate {..} =
  Coordinate (row `mod` board.height) (column `mod` board.width)

initialState :: Board -> WalkState UnitSquare
initialState board =
  WalkState
    { position =
        Coordinate
          { row = 0
          , column =
              fromJust $ findIndex ((`isOnBoard` board) . Coordinate 0) [0 ..]
          }
    , facing = East
    }

moveWithWrap :: Board -> WalkState UnitSquare -> Maybe (WalkState UnitSquare)
moveWithWrap board WalkState {..} = go position
  where
    go coordinate =
      let oneAhead = shiftInside board $ coordinate `moveTowards` facing
       in case board `atCoordinate` oneAhead of
            Void -> go $ moveTowardsBy coordinate facing $ sideLength board
            Open -> Just $ WalkState oneAhead facing
            Wall -> Nothing
