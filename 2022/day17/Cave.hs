module Cave where

import           Control.Monad.State (State, gets, modify)
import           Coordinate          (Coordinate (..))
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Direction           (Direction (South))
import           Jet
import           Rock

type Cave = Set Coordinate

isInsideWall :: Coordinate -> Bool
isInsideWall Coordinate {..} = column == 0 || column == 8

emptyCave :: Cave
emptyCave = S.fromList [Coordinate 0 col | col <- [1 .. 7]]

topRow :: Cave -> Int
topRow = row . S.findMin

belongsTo :: Coordinate -> Cave -> Bool
belongsTo = S.member

withRock :: Cave -> Rock -> Cave
cave `withRock` Rock {..} = cave `S.union` S.fromList getRocks

conflictsWith :: Rock -> Cave -> Bool
rock `conflictsWith` cave =
  rock `hasPartWith` (\c -> isInsideWall c || c `belongsTo` cave)

getHeights :: [Shape] -> [Jet] -> State Cave [Int]
getHeights [] _ = return []
getHeights _ [] = return []
getHeights (nextShape:shapes) jets = do
  currentTop <- gets topRow
  let nextRock = makeRock (Coordinate (currentTop - 4) 3) nextShape
  remainingJets <- dropRock nextRock jets
  newTop <- gets topRow
  otherheights <- getHeights shapes remainingJets
  return $ negate newTop : otherheights
  where
    dropRock :: Rock -> [Jet] -> State Cave [Jet]
    dropRock _ [] = return [] -- This is not supposed to happen for the real input
    dropRock fallingRock (jet:otherJets) = do
      let shiftedRock = fallingRock `moveRockTowards` toDirection jet
      hitsSomething <- gets (shiftedRock `conflictsWith`)
      let pushedRock =
            if hitsSomething
              then fallingRock
              else shiftedRock
          fallenRock = pushedRock `moveRockTowards` South
      comesToRest <- gets (fallenRock `conflictsWith`)
      if comesToRest
        then modify (`withRock` pushedRock) >> return otherJets
        else dropRock fallenRock otherJets
