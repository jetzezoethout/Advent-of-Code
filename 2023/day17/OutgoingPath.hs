module OutgoingPath where

import           City (City)
import           Grid (atCoordinate, isInside)
import           Node (Node (coordinate), targetLocations)

data OutgoingPath = OutgoingPath
  { cost   :: Int
  , target :: Node
  } deriving (Show)

processTargetSequence :: City -> [Node] -> [OutgoingPath]
processTargetSequence city nodes =
  zipWith
    OutgoingPath
    (scanl1 (+)
       $ map (city `atCoordinate`)
       $ filter (`isInside` city)
       $ map coordinate nodes)
    nodes

getPathsFrom :: Node -> City -> [OutgoingPath]
getPathsFrom node city = targetLocations 3 node >>= processTargetSequence city

getUltraPathsFrom :: Node -> City -> [OutgoingPath]
getUltraPathsFrom node city =
  targetLocations 10 node >>= drop 3 . processTargetSequence city
