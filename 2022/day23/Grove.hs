module Grove where

import           Control.Monad (guard)
import           Coordinate    (Coordinate (..))
import           Data.List     (find)
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Direction     (Direction (..), allDirections, clockWise,
                                counterClockWise, moveTowards)
import           TaggedRow     (TaggedRow (..), parseTaggedLines)

type Grove = Set Coordinate

parseGrove :: Text -> Set Coordinate
parseGrove text = S.fromList $ parseTaggedLines text >>= getElves
  where
    getElves TaggedRow {..} =
      map (Coordinate rowIndex . fst)
        $ filter ((== '#') . snd)
        $ zip [0 ..]
        $ T.unpack content

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
      case spreadOrFinish preferredDirs grove of
        Nothing -> acc + 1
        Just newGrove ->
          go (tail preferredDirs <> [head preferredDirs]) (acc + 1) newGrove

spreadTimes :: Int -> Grove -> Grove
spreadTimes = go [North, South, West, East]
  where
    go :: [Direction] -> Int -> Grove -> Grove
    go _ 0 = id
    go preferredDirs n =
      go (tail preferredDirs <> [head preferredDirs]) (n - 1)
        . spread preferredDirs

spread :: [Direction] -> Grove -> Grove
spread preferredDirs grove =
  fromMaybe grove $ spreadOrFinish preferredDirs grove

spreadOrFinish :: [Direction] -> Grove -> Maybe Grove
spreadOrFinish preferredDirs grove =
  case S.foldr' addElf (0, S.empty) grove of
    (0, _)        -> Nothing
    (_, newGrove) -> Just newGrove
  where
    elf `canMoveTo` dir =
      all (`S.notMember` grove) $ elf `coordinatesTowards` dir
    movingDir elf =
      guard (any (`S.member` grove) (neighbours elf))
        >> find (elf `canMoveTo`) preferredDirs
    addElf :: Coordinate -> (Int, Grove) -> (Int, Grove)
    addElf elf (movements, acc) =
      case movingDir elf of
        Nothing -> (movements, S.insert elf acc)
        Just dir ->
          let target = elf `moveTowards` dir
           in if target `S.member` acc
                then ( movements - 1
                     , S.insert elf
                         $ S.insert (target `moveTowards` dir)
                         $ S.delete target acc)
                else (movements + 1, S.insert target acc)

coordinatesTowards :: Coordinate -> Direction -> [Coordinate]
coord `coordinatesTowards` dir =
  map
    ($ (coord `moveTowards` dir))
    [id, (`moveTowards` clockWise dir), (`moveTowards` counterClockWise dir)]

neighbours :: Coordinate -> [Coordinate]
neighbours coord = allDirections >>= forwardAndRight
  where
    forwardAndRight dir =
      map ($ (coord `moveTowards` dir)) [id, (`moveTowards` clockWise dir)]
