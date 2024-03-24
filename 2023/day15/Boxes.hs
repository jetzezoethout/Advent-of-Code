module Boxes
  ( processAll
  , score
  ) where

import           Box                 (Box, boxScore, emptyBox)
import           Control.Monad.State (State, execState, gets, modify)
import           Data.Map.Strict     (Map, fromList, mapWithKey, (!))
import qualified Data.Map.Strict     as M
import           Instruction         (Instruction (..))

type Boxes = Map Int Box

initialBoxes :: Boxes
initialBoxes = fromList $ map (, emptyBox) [0 .. 255]

process :: Instruction -> State Boxes ()
process Instruction {..} = do
  boxToEdit <- gets (! boxNumber)
  let newBox = operation boxToEdit
  modify $ M.insert boxNumber newBox

processAll :: [Instruction] -> Boxes
processAll instructions = execState (mapM_ process instructions) initialBoxes

score :: Boxes -> Int
score = sum . mapWithKey (\nr box -> (nr + 1) * boxScore box)
