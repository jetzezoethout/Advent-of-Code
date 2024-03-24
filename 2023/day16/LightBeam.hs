module LightBeam where

import           Cave                 (Cave, possibleEntryPairs)
import           Control.Monad        (join, when)
import           Control.Monad.Reader (MonadReader (reader),
                                       ReaderT (runReaderT))
import           Control.Monad.State  (State, execState, gets, modify)
import           Coordinate           (Coordinate (..))
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Direction            (Direction (East), invert, moveTowards)
import           Grid                 (isInside, safeAtCoordinate)
import           LightDeflector       (exitDirections)

data LightBeam = LightBeam
  { location      :: Coordinate
  , movingTowards :: Direction
  } deriving (Eq, Ord, Show)

initialBeam :: LightBeam
initialBeam = LightBeam (Coordinate 0 0) East

invertBeam :: LightBeam -> LightBeam
invertBeam LightBeam {..} =
  LightBeam {location = location, movingTowards = invert movingTowards}

exits :: LightBeam -> Cave -> Bool
LightBeam {..} `exits` cave =
  not ((location `moveTowards` movingTowards) `isInside` cave)

type CaveContext = ReaderT Cave (State (Set LightBeam))

propagate :: LightBeam -> CaveContext ()
propagate beam@LightBeam {..} = do
  isInsideCave <- reader (location `isInside`)
  alreadyEncountered <- gets $ S.member beam
  when (isInsideCave && not alreadyEncountered) $ do
    modify $ S.insert beam
    optionalDeflector <- reader $ join . (`safeAtCoordinate` location)
    mapM_
      (propagate . (\dir -> LightBeam (location `moveTowards` dir) dir))
      (exitDirections movingTowards optionalDeflector)

getLightBeams :: LightBeam -> Cave -> Set LightBeam
getLightBeams beam cave = execState (runReaderT (propagate beam) cave) S.empty

getEnergizedTiles :: LightBeam -> Cave -> Int
getEnergizedTiles beam = S.size . S.map location . getLightBeams beam

-- If a beam exits the cave, we can skip checking the corresponding entrypoint
getMaxEnergizable :: Cave -> Int
getMaxEnergizable cave =
  go $ S.fromList $ map (uncurry LightBeam) $ possibleEntryPairs cave
  where
    go beamsToCheck =
      if S.size beamsToCheck == 0
        then 0
        else let entryBeam = S.elemAt 0 beamsToCheck
                 resultingBeams = getLightBeams entryBeam cave
                 beamsAtEdge = S.filter (`exits` cave) resultingBeams
                 beamsLeftToCheck =
                   S.deleteAt 0 beamsToCheck
                     `S.difference` S.map invertBeam beamsAtEdge
              in max (S.size $ S.map location resultingBeams)
                   $ go beamsLeftToCheck
