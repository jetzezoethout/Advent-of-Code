module BurrowTypes where

import           Amphipod      (Amphipod, allAmphipods, energy, fromChar,
                                targetColumn)
import           Burrow        (Burrow (..), Move (..))
import           Chamber       (BurrowDepth, Chamber, ChamberType (..),
                                Hallway (..), SideRoom (..), allSideRooms,
                                hallwayDistance, hallwayEntrance, legalHallways)
import           Control.Monad (guard)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (mapMaybe, maybeToList)
import           Data.Text     (Text)
import qualified Data.Text     as T

type AmphipodMap = Map Chamber Amphipod

newtype SmallBurrow =
  SmallBurrow AmphipodMap
  deriving (Eq, Ord, Show)

instance Burrow SmallBurrow where
  goal :: SmallBurrow
  goal = SmallBurrow $ goalMap 2
  possibleMoves :: SmallBurrow -> [Move SmallBurrow]
  possibleMoves (SmallBurrow amphipodMap) =
    map (fmap SmallBurrow) $ possibleMovesFromMap 2 amphipodMap
  heuristic :: SmallBurrow -> Int
  heuristic (SmallBurrow amphipodMap) = heuristicFromMap 2 amphipodMap

parseSmallBurrow :: Text -> SmallBurrow
parseSmallBurrow text =
  let textLines = T.lines text
   in SmallBurrow
        $ M.fromList
            [ ( toChamber $ SideRoom owner depth
              , fromChar
                  $ (textLines !! (depth + 1))
                      `T.index` (targetColumn owner + 1))
            | depth <- [1, 2]
            , owner <- allAmphipods
            ]

newtype LargeBurrow =
  LargeBurrow AmphipodMap
  deriving (Eq, Ord, Show)

instance Burrow LargeBurrow where
  goal :: LargeBurrow
  goal = LargeBurrow $ goalMap 4
  possibleMoves :: LargeBurrow -> [Move LargeBurrow]
  possibleMoves (LargeBurrow amphipodMap) =
    map (fmap LargeBurrow) $ possibleMovesFromMap 4 amphipodMap
  heuristic :: LargeBurrow -> Int
  heuristic (LargeBurrow amphipodMap) = heuristicFromMap 4 amphipodMap

parseLargeBurrow :: Text -> LargeBurrow
parseLargeBurrow text =
  let textLines = T.lines text
   in LargeBurrow
        $ M.fromList
        $ [ ( toChamber $ SideRoom owner 1
            , fromChar $ (textLines !! 2) `T.index` (targetColumn owner + 1))
          | owner <- allAmphipods
          ]
            <> [ (toChamber $ SideRoom owner 2, fromChar char)
               | (owner, char) <- zip allAmphipods "DCBA"
               ]
            <> [ (toChamber $ SideRoom owner 3, fromChar char)
               | (owner, char) <- zip allAmphipods "DBAC"
               ]
            <> [ ( toChamber $ SideRoom owner 4
                 , fromChar
                     $ (textLines !! 3) `T.index` (targetColumn owner + 1))
               | owner <- allAmphipods
               ]

goalMap :: BurrowDepth -> AmphipodMap
goalMap maxDepth =
  M.fromList
    [ (toChamber $ SideRoom owner depth, owner)
    | depth <- [1 .. maxDepth]
    , owner <- allAmphipods
    ]

canMoveThroughHallway :: AmphipodMap -> Hallway -> Hallway -> Bool
canMoveThroughHallway amphipodMap (Hallway source) (Hallway target) =
  all ((`M.notMember` amphipodMap) . toChamber . Hallway) requiredFreeSpaces
  where
    requiredFreeSpaces =
      if source < target
        then [source + 1 .. target]
        else [target .. source - 1]

reachableHallwayPositions :: AmphipodMap -> Hallway -> [Hallway]
reachableHallwayPositions amphipodMap hallway =
  filter (canMoveThroughHallway amphipodMap hallway) legalHallways

sideRoomsContainForeigners :: BurrowDepth -> AmphipodMap -> Amphipod -> Bool
sideRoomsContainForeigners maxDepth amphipodMap owner =
  any containsForeigner [SideRoom owner depth | depth <- [1 .. maxDepth]]
  where
    containsForeigner sideRoom =
      case M.lookup (toChamber sideRoom) amphipodMap of
        Just amphipod -> amphipod /= owner
        Nothing       -> False

moveFromSideRoomToHallway ::
     BurrowDepth -> AmphipodMap -> SideRoom -> [Move AmphipodMap]
moveFromSideRoomToHallway maxDepth amphipodMap sideRoom = do
  amphipod <- maybeToList $ M.lookup (toChamber sideRoom) amphipodMap
  guard $ shouldMoveToHallway amphipod
  guard canMoveToHallway
  let halfwayPoint = hallwayEntrance sideRoom
  targetHallway <- reachableHallwayPositions amphipodMap halfwayPoint
  let distance = sideRoom.depth + hallwayDistance halfwayPoint targetHallway
  return
    $ Move
        (distance * energy amphipod)
        (M.insert (toChamber targetHallway) amphipod
           $ M.delete (toChamber sideRoom) amphipodMap)
  where
    shouldMoveToHallway amphipod =
      amphipod /= sideRoom.owner
        || sideRoomsContainForeigners maxDepth amphipodMap amphipod
    canMoveToHallway =
      all
        ((`M.notMember` amphipodMap) . toChamber . SideRoom sideRoom.owner)
        [1 .. sideRoom.depth - 1]

moveFromHallwayToSideRoom ::
     BurrowDepth -> AmphipodMap -> Hallway -> Maybe (Move AmphipodMap)
moveFromHallwayToSideRoom maxDepth amphipodMap hallway = do
  amphipod <- M.lookup (toChamber hallway) amphipodMap
  guard $ not $ sideRoomsContainForeigners maxDepth amphipodMap amphipod
  let halfwayPoint = Hallway $ targetColumn amphipod
  guard $ canMoveThroughHallway amphipodMap hallway halfwayPoint
  targetSideRoom <-
    safeLast
      $ takeWhile ((`M.notMember` amphipodMap) . toChamber)
      $ [SideRoom amphipod depth | depth <- [1 .. maxDepth]]
  let distance = hallwayDistance hallway halfwayPoint + targetSideRoom.depth
  return
    $ Move
        (distance * energy amphipod)
        (M.insert (toChamber targetSideRoom) amphipod
           $ M.delete (toChamber hallway) amphipodMap)
  where
    safeLast []     = Nothing
    safeLast [x]    = Just x
    safeLast (_:xs) = safeLast xs

possibleMovesFromMap :: BurrowDepth -> AmphipodMap -> [Move AmphipodMap]
possibleMovesFromMap maxDepth amphipodMap =
  possibilitiesFromSideRoomToHallway <> possibilitiesFromHallwayToSideRoom
  where
    possibilitiesFromSideRoomToHallway =
      allSideRooms maxDepth >>= moveFromSideRoomToHallway maxDepth amphipodMap
    possibilitiesFromHallwayToSideRoom =
      mapMaybe (moveFromHallwayToSideRoom maxDepth amphipodMap) legalHallways

heuristicFromMap :: BurrowDepth -> AmphipodMap -> Int
heuristicFromMap maxDepth amphipodMap =
  moveIntoRooms
    + sum (mapMaybe distanceFromHallway legalHallways)
    + sum (mapMaybe distanceFromSideRoom $ allSideRooms maxDepth)
  where
    -- For each amphipod, we will compute the cost of walking to its hallway
    -- entrance, where an amphipod has negative cost if it is already in its
    -- own sideroom. Then, we add the total cost of letting all amphipods
    -- fill their siderooms, starting from their hallway entrances
    distanceFromHallway :: Hallway -> Maybe Int
    distanceFromHallway hallway = do
      amphipod <- M.lookup (toChamber hallway) amphipodMap
      return
        $ energy amphipod
            * hallwayDistance hallway (Hallway $ targetColumn amphipod)
    distanceFromSideRoom :: SideRoom -> Maybe Int
    distanceFromSideRoom sideRoom = do
      amphipod <- M.lookup (toChamber sideRoom) amphipodMap
      return
        $ energy amphipod
            * (if amphipod == sideRoom.owner
                 then -sideRoom.depth
                 else sideRoom.depth
                        + abs
                            (targetColumn amphipod - targetColumn sideRoom.owner))
    moveIntoRooms :: Int
    moveIntoRooms =
      sum (map energy allAmphipods) * maxDepth * (maxDepth + 1) `div` 2
