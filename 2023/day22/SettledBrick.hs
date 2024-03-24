module SettledBrick where

import           Brick         (Brick)
import           Data.Function (on)
import           Data.List     (sortBy)

type BrickId = Int

data SettledBrick = SettledBrick
  { brickId :: BrickId
  , brick   :: Brick
  , restsOn :: [BrickId]
  } deriving (Show)

yeetBrick :: BrickId -> [SettledBrick] -> Int
yeetBrick yeetedBrickId = go [yeetedBrickId]
  where
    go yeeted [] = length yeeted - 1
    go yeeted (SettledBrick {..}:otherBricks) =
      go
        (if not (null restsOn) && all (`elem` yeeted) restsOn
           then brickId : yeeted
           else yeeted)
        otherBricks

yeetAllBricks :: [SettledBrick] -> [Int]
yeetAllBricks = go . sortBy (compare `on` brickId)
  where
    go [] = []
    go (brickToYeet:otherBricks) =
      yeetBrick (brickId brickToYeet) otherBricks : go otherBricks
