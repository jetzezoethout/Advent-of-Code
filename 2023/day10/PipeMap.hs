module PipeMap where

import           Coordinate (Coordinate (..))
import           Data.Maybe (isJust)
import           Data.Text  (Text)
import           Direction  (Direction, allDirections, moveTowards)
import           Grid       (Grid (..), atCoordinate, parseGrid,
                             safeAtCoordinate)
import           Pipe       (Pipe (Start), continue, fromChar)

type PipeMap = Grid Pipe

parsePipeMap :: Text -> PipeMap
parsePipeMap = parseGrid fromChar

findStart :: PipeMap -> Coordinate
findStart pipeMap =
  let coordinates =
        [ Coordinate r c
        | r <- [0 .. pipeMap.height - 1]
        , c <- [0 .. pipeMap.width - 1]
        ]
   in head $ filter ((== Start) . (pipeMap `atCoordinate`)) coordinates

findSomewhereToGo :: PipeMap -> Coordinate -> Direction
findSomewhereToGo pipeMap coordinate =
  head
    $ filter
        (\dir ->
           isJust
             $ pipeMap `safeAtCoordinate` (coordinate `moveTowards` dir)
                 >>= continue dir)
        allDirections
