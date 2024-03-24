module HikingMap where

import           Coordinate    (Coordinate (..))
import           Data.Maybe    (mapMaybe)
import           Data.Text     (Text)
import           Direction     (allDirections, moveTowards)
import           Grid          (Grid (..), atCoordinate, isInside, parseGrid)
import           HikingElement (HikingElement (..), fromChar,
                                fromCharNonSlippery)

type HikingMap = Grid HikingElement

parseHikingMap :: Text -> Grid HikingElement
parseHikingMap = parseGrid fromChar

parseNonSlipperyMap :: Text -> HikingMap
parseNonSlipperyMap = parseGrid fromCharNonSlippery

isAccessible :: HikingMap -> Coordinate -> Bool
isAccessible hikingMap coord =
  coord `isInside` hikingMap && hikingMap `atCoordinate` coord /= Forest

possibleNextSteps :: HikingMap -> Coordinate -> [Coordinate]
possibleNextSteps hikingMap coord =
  let possibilities =
        case hikingMap `atCoordinate` coord of
          Forest    -> []
          Path      -> map (coord `moveTowards`) allDirections
          Slope dir -> [coord `moveTowards` dir]
   in filter (isAccessible hikingMap) possibilities

isNode :: HikingMap -> Coordinate -> Bool
isNode hikingMap coord =
  hikingMap `atCoordinate` coord /= Forest
    && length
         (filter (isAccessible hikingMap)
            $ map (coord `moveTowards`) allDirections)
         /= 2

allNodes :: HikingMap -> [Coordinate]
allNodes hikingMap =
  filter
    (isNode hikingMap)
    [ Coordinate i j
    | i <- [0 .. height hikingMap - 1]
    , j <- [0 .. width hikingMap - 1]
    ]

walkToNodes :: HikingMap -> Coordinate -> [(Coordinate, Int)]
walkToNodes hikingMap coord =
  mapMaybe (go 1 coord)
    $ filter (isAccessible hikingMap)
    $ map (coord `moveTowards`) allDirections
  where
    go :: Int -> Coordinate -> Coordinate -> Maybe (Coordinate, Int)
    go distanceSoFar previous current =
      if isNode hikingMap current
        then Just (current, distanceSoFar)
        else case filter (/= previous) $ possibleNextSteps hikingMap current of
               [onlyPossibility] ->
                 go (distanceSoFar + 1) current onlyPossibility
               _ -> Nothing
