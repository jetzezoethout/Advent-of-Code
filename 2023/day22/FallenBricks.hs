module FallenBricks where

import           Brick               (Brick, dropToZ, groundShadow)
import           Control.Monad.State (State, execState, gets, modify)
import           Data.Function       (on)
import           Data.List           (groupBy, nub, sortBy)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (mapMaybe)
import           SettledBrick        (BrickId, SettledBrick (..))
import           VerticalSlice       (Towers, VerticalSlice (..),
                                      getOrElseEmpty, putBrick)

data FallenBricks = FallenBricks
  { settledBricks :: [SettledBrick]
  , towers        :: Towers
  } deriving (Show)

initialState :: FallenBricks
initialState = FallenBricks [] M.empty

type FallingBricksContext = State FallenBricks

modifyBricks :: ([SettledBrick] -> [SettledBrick]) -> FallingBricksContext ()
modifyBricks f =
  modify $ \FallenBricks {..} ->
    FallenBricks {settledBricks = f settledBricks, towers = towers}

modifyTowers :: (Towers -> Towers) -> FallingBricksContext ()
modifyTowers f =
  modify $ \FallenBricks {..} ->
    FallenBricks {settledBricks = settledBricks, towers = f towers}

dropBrick :: (BrickId, Brick) -> FallingBricksContext ()
dropBrick (brickId, brick) = do
  let shadow = groundShadow brick
  towersToCheck <- mapM (\coord -> gets (getOrElseEmpty coord . towers)) shadow
  let supportingTowers =
        head
          $ groupBy ((==) `on` highestOccupiedZ)
          $ sortBy (flip compare `on` highestOccupiedZ) towersToCheck
      targetZ = highestOccupiedZ (head supportingTowers) + 1
      settledBrick =
        SettledBrick
          { brickId = brickId
          , brick = dropToZ targetZ brick
          , restsOn = nub $ mapMaybe highestBrickId supportingTowers
          }
  modifyBricks (settledBrick :)
  mapM_ (\coord -> modifyTowers $ M.insert coord $ putBrick settledBrick) shadow

dropAllBricks :: [Brick] -> [SettledBrick]
dropAllBricks bricks =
  settledBricks $ execState (mapM dropBrick $ zip [0 ..] bricks) initialState
