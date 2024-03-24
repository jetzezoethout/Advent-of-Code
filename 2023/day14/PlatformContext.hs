module PlatformContext where

import           Control.Monad.Reader (MonadReader (reader), ReaderT)
import           Control.Monad.State  (MonadState (get), State, gets, modify)
import           Coordinate           (Coordinate (..))
import           CycledList           (CycledList (..), atIndex)
import           Data.Function        (on)
import           Data.List            (elemIndex, sortBy)
import           Data.Set             (delete, insert, member, toList)
import           Direction            (Direction (..), moveTowards)
import           Grid                 (Grid (..), atCoordinate, isInside)
import           RockContainers       (FixedRockTeller, RollingRocks)

type PlatformContext = ReaderT FixedRockTeller (State RollingRocks)

score :: PlatformContext Int
score = do
  platformHeight <- reader height
  rollingRockList <- gets toList
  return $ sum $ map ((platformHeight -) . row) rollingRockList

sortCoordinates :: Direction -> Coordinate -> Coordinate -> Ordering
sortCoordinates direction =
  case direction of
    North -> compare `on` row
    West  -> compare `on` column
    South -> flip compare `on` row
    East  -> flip compare `on` column

findFurthestFreeSpot :: Direction -> Coordinate -> PlatformContext Coordinate
findFurthestFreeSpot direction coordinate = do
  let newCoordinate = coordinate `moveTowards` direction
  isOnPlatform <- reader (newCoordinate `isInside`)
  hasFixedRock <- reader (`atCoordinate` newCoordinate)
  hasRollingRock <- gets $ member newCoordinate
  if not isOnPlatform || hasFixedRock || hasRollingRock
    then return coordinate
    else findFurthestFreeSpot direction newCoordinate

rollRockTowards :: Direction -> Coordinate -> PlatformContext ()
rollRockTowards direction coordinate = do
  target <- findFurthestFreeSpot direction coordinate
  modify $ delete coordinate
  modify $ insert target

rollTowards :: Direction -> PlatformContext ()
rollTowards direction = do
  rocksToRoll <- gets $ sortBy (sortCoordinates direction) . toList
  mapM_ (rollRockTowards direction) rocksToRoll

cycleOnce :: PlatformContext ()
cycleOnce = mapM_ rollTowards [North, West, South, East]

findCycle :: PlatformContext (CycledList RollingRocks)
findCycle = go []
  where
    go observed = do
      currentState <- get
      case elemIndex currentState observed of
        Nothing -> cycleOnce >> go (currentState : observed)
        Just index ->
          let (revCycle, revInitial) = splitAt (index + 1) observed
           in return $ CycledList (reverse revInitial) (reverse revCycle)

resultOfCycling :: Int -> PlatformContext RollingRocks
resultOfCycling times = do
  cycledList <- findCycle
  return $ cycledList `atIndex` times
