module Chamber where

import           Amphipod (Amphipod, allAmphipods, targetColumn)

type BurrowDepth = Int

newtype Hallway =
  Hallway Int
  deriving (Eq, Ord, Show)

legalHallways :: [Hallway]
legalHallways = map Hallway [0, 1, 3, 5, 7, 9, 10]

hallwayDistance :: Hallway -> Hallway -> Int
hallwayDistance (Hallway source) (Hallway target) = abs $ source - target

data SideRoom = SideRoom
  { owner :: Amphipod
  , depth :: Int
  } deriving (Eq, Ord, Show)

hallwayEntrance :: SideRoom -> Hallway
hallwayEntrance = Hallway . targetColumn . owner

allSideRooms :: BurrowDepth -> [SideRoom]
allSideRooms maxDepth =
  [SideRoom owner depth | owner <- allAmphipods, depth <- [1 .. maxDepth]]

type Chamber = Either Hallway SideRoom

class ChamberType a where
  toChamber :: a -> Chamber

instance ChamberType Hallway where
  toChamber :: Hallway -> Chamber
  toChamber = Left

instance ChamberType SideRoom where
  toChamber :: SideRoom -> Chamber
  toChamber = Right
