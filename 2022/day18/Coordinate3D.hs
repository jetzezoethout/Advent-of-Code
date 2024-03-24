module Coordinate3D where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Coordinate3D = Coordinate3D
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show, Eq, Ord)

parseCoordinate3D :: Text -> Coordinate3D
parseCoordinate3D text =
  let parts = map parseUnsignedInt $ T.splitOn "," text
   in Coordinate3D (head parts) (parts !! 1) (parts !! 2)

neighbours :: Coordinate3D -> [Coordinate3D]
neighbours coordinate =
  [ coordinate {x = coordinate.x - 1}
  , coordinate {x = coordinate.x + 1}
  , coordinate {y = coordinate.y - 1}
  , coordinate {y = coordinate.y + 1}
  , coordinate {z = coordinate.z - 1}
  , coordinate {z = coordinate.z + 1}
  ]
