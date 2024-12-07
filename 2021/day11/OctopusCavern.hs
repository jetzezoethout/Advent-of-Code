module OctopusCavern where

import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.State (State, evalState, gets, modify)
import           Coordinate          (Coordinate (Coordinate), addCoordinate)
import           Data.Foldable       (traverse_)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import           LocatedChar         (LocatedChar (..), locateText)

type OctopusCavern = Map Coordinate Int

parseOctopusCavern :: Text -> Map Coordinate Int
parseOctopusCavern = M.fromList . map parseEnergy . locateText
  where
    parseEnergy LocatedChar {..} = (location, fromEnum char - fromEnum '0')

flashForever :: OctopusCavern -> [Int]
flashForever = evalState go
  where
    go = liftA2 (:) oneRound go

oneRound :: State OctopusCavern Int
oneRound = loadEnergy >> flash >>= resetEnergy

loadEnergy :: State OctopusCavern ()
loadEnergy = modify $ M.map (+ 1)

flash :: State OctopusCavern (Set Coordinate)
flash = go S.empty
  where
    go :: Set Coordinate -> State OctopusCavern (Set Coordinate)
    go flashed = do
      flashyOctopuses <- gets $ M.keys . M.filter (> 9)
      case filter (`S.notMember` flashed) flashyOctopuses of
        [] -> return flashed
        nonempty -> do
          traverse_ processFlash nonempty
          go $ flashed `S.union` S.fromList nonempty
    processFlash :: Coordinate -> State OctopusCavern ()
    processFlash = traverse_ (modify . M.adjust (+ 1)) . neighbours

resetEnergy :: Set Coordinate -> State OctopusCavern Int
resetEnergy flashed = do
  traverse_ (modify . (`M.insert` 0)) flashed
  return $ length flashed

neighbours :: Coordinate -> [Coordinate]
neighbours coord = map (addCoordinate coord) deltas
  where
    deltas =
      [ Coordinate row col
      | row <- [-1, 0, 1]
      , col <- [-1, 0, 1]
      , not (row == 0 && col == 0)
      ]
