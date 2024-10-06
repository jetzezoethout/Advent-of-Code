module SeaMonster where

import           Coordinate  (Coordinate (..), addCoordinate)
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Puzzle      (Puzzle)
import           Reflectable (Reflectable (..), reflectAntiDiagonal,
                              reflectColumns, reflectDiagonal)
import           Rotatable   (Rotatable (..), rotateCounterClockwise,
                              rotateFullTurn)

newtype SeaMonster = SeaMonster
  { monster :: Set Coordinate
  }

basicMonster :: SeaMonster
basicMonster =
  SeaMonster
    $ S.fromList
    $ [Coordinate (-1) 18]
        <> [Coordinate 0 col | col <- [0, 5, 6, 11, 12, 17, 18, 19]]
        <> [Coordinate 1 col | col <- [1, 4, 7, 10, 13, 16]]

allMonsters :: [SeaMonster]
allMonsters =
  map
    ($ basicMonster)
    [ id
    , rotateClockWise
    , rotateFullTurn
    , rotateCounterClockwise
    , reflectRows
    , reflectColumns
    , reflectDiagonal
    , reflectAntiDiagonal
    ]

instance Rotatable SeaMonster where
  rotateClockWise :: SeaMonster -> SeaMonster
  rotateClockWise (SeaMonster monster) = SeaMonster $ S.map rotateSquare monster
    where
      rotateSquare Coordinate {..} = Coordinate {row = column, column = -row}

instance Reflectable SeaMonster where
  reflectRows :: SeaMonster -> SeaMonster
  reflectRows (SeaMonster monster) = SeaMonster $ S.map reflectSquare monster
    where
      reflectSquare Coordinate {..} = Coordinate {row = -row, column = column}

monsterImprintAt :: Puzzle -> SeaMonster -> Coordinate -> Set Coordinate
monsterImprintAt puzzle (SeaMonster monster) coord =
  let movedMonster = S.map (addCoordinate coord) monster
   in if movedMonster `S.isSubsetOf` puzzle
        then movedMonster
        else S.empty

monsterImprints :: Puzzle -> SeaMonster -> Set Coordinate
monsterImprints puzzle seaMonster =
  S.foldl' S.union S.empty $ S.map (monsterImprintAt puzzle seaMonster) puzzle

monsterFree :: Puzzle -> Int
monsterFree puzzle =
  S.size puzzle
    - head (filter (> 0) $ map (S.size . monsterImprints puzzle) allMonsters)
