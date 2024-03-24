module RopeJourney where

import           Coordinate (Coordinate)
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Direction  (Direction (..))
import           Parsers    (parseUnsignedInt)
import           Rope       (Rope, moveRopeTowards, ropeTail)

parseDirections :: Text -> [Direction]
parseDirections text = T.lines text >>= parseDirectionLine

parseDirectionLine :: Text -> [Direction]
parseDirectionLine text =
  let parts = T.words text
   in replicate (parseUnsignedInt $ parts !! 1) (parseDirection $ head parts)

parseDirection :: Text -> Direction
parseDirection "R" = East
parseDirection "U" = North
parseDirection "L" = West
parseDirection "D" = South
parseDirection _   = error "not a direction"

tailLocations :: Rope -> [Direction] -> Int
tailLocations rope directions = go rope directions $ S.singleton $ ropeTail rope
  where
    go :: Rope -> [Direction] -> Set Coordinate -> Int
    go _ [] visitedSoFar = S.size visitedSoFar
    go currentRope (nextDirection:otherDirections) visitedSoFar =
      let nextRope = currentRope `moveRopeTowards` nextDirection
       in go nextRope otherDirections
            $ S.insert (ropeTail nextRope) visitedSoFar
