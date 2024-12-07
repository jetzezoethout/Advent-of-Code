module Grove where

import           Control.Monad (guard)
import           Coordinate    (Coordinate (..), addCoordinates)
import           Data.List     (find)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text     (Text)
import           Direction     (Direction (..), moveTowards)
import           LocatedChar   (LocatedChar (..), locateText)

type Grove = Set Coordinate

parseGrove :: Text -> Set Coordinate
parseGrove = S.fromList . map location . filter ((== '#') . char) . locateText

emptySpaces :: Set Coordinate -> Int
emptySpaces grove =
  let minRow = row $ S.findMin grove
      maxRow = row $ S.findMax grove
      columns = S.map column grove
      minCol = S.findMin columns
      maxCol = S.findMax columns
   in (maxRow - minRow + 1) * (maxCol - minCol + 1) - S.size grove

roundsNeeded :: Grove -> Int
roundsNeeded = go [North, South, West, East] 0
  where
    go preferredDirs acc grove =
      let newGrove = spread preferredDirs grove
       in if grove == newGrove
            then acc + 1
            else go
                   (tail preferredDirs <> [head preferredDirs])
                   (acc + 1)
                   newGrove

spreadTimes :: Int -> Grove -> Grove
spreadTimes = go [North, South, West, East]
  where
    go :: [Direction] -> Int -> Grove -> Grove
    go _ 0 = id
    go preferredDirs n =
      go (tail preferredDirs <> [head preferredDirs]) (n - 1)
        . spread preferredDirs

spread :: [Direction] -> Grove -> Grove
spread preferredDirs grove = S.foldr' addElf grove grove
  where
    elf `canMoveTo` dir =
      all (`S.notMember` grove) $ elf `coordinatesTowards` dir
    movingDir elf =
      guard (any (`S.member` grove) (neighbours elf))
        >> find (elf `canMoveTo`) preferredDirs
    addElf :: Coordinate -> Grove -> Grove
    addElf elf acc =
      case movingDir elf of
        Nothing -> acc
        Just dir ->
          let target = elf `moveTowards` dir
           in if target `S.member` acc
                then S.insert (target `moveTowards` dir) $ S.delete target acc
                else S.insert target $ S.delete elf acc

coordinatesTowards :: Coordinate -> Direction -> [Coordinate]
coord `coordinatesTowards` dir = map (addCoordinates coord) deltas
  where
    deltas =
      case dir of
        North -> [Coordinate (-1) col | col <- [-1, 0, 1]]
        East  -> [Coordinate row 1 | row <- [-1, 0, 1]]
        South -> [Coordinate 1 col | col <- [-1, 0, 1]]
        West  -> [Coordinate row (-1) | row <- [-1, 0, 1]]

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (addCoordinates coord) deltas
  where
    deltas =
      [ Coordinate row col
      | row <- [-1, 0, 1]
      , col <- [-1, 0, 1]
      , not (row == 0 && col == 0)
      ]
