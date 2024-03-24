module Cave where

import           Coordinate (Coordinate (..))
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseInt)

type Cave = Set Coordinate

sandOrigin :: Coordinate
sandOrigin = Coordinate 0 500

highestRow :: Set Coordinate -> Int
highestRow = row . S.findMax

getSegment :: Coordinate -> Coordinate -> [Coordinate]
getSegment c1 c2 =
  if c1.row == c2.row
    then [ Coordinate c1.row column
         | column <- [min c1.column c2.column .. max c1.column c2.column]
         ]
    else [ Coordinate row c1.column
         | row <- [min c1.row c2.row .. max c1.row c2.row]
         ]

parseCorners :: Text -> [Coordinate]
parseCorners = map parseCoordinate . T.splitOn " -> "
  where
    parseCoordinate pair =
      let parts = T.splitOn "," pair
       in Coordinate (parseInt $ parts !! 1) (parseInt $ head parts)

parseCaveSegement :: Text -> Set Coordinate
parseCaveSegement text =
  let corners = parseCorners text
   in S.unions $ map S.fromList $ zipWith getSegment corners $ tail corners

parseCave :: Text -> Set Coordinate
parseCave = S.unions . map parseCaveSegement . T.lines
