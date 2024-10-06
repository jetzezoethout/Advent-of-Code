module Constraint where

import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show)

parseRange :: Text -> Range
parseRange text =
  let parts = T.splitOn "-" text
   in Range
        { start = parseUnsignedInt $ head parts
        , end = parseUnsignedInt $ parts !! 1
        }

isInside :: Int -> Range -> Bool
x `isInside` Range {..} = start <= x && x <= end

data Constraint = Constraint
  { range1 :: Range
  , range2 :: Range
  } deriving (Show)

type Constraints = Map Text Constraint

parseConstraints :: [Text] -> Constraints
parseConstraints = M.fromList . map parseLine
  where
    parseLine text =
      let parts = T.splitOn ": " text
       in (head parts, parseConstraint $ parts !! 1)

parseConstraint :: Text -> Constraint
parseConstraint text =
  let ranges = T.splitOn " or " text
   in Constraint
        {range1 = parseRange $ head ranges, range2 = parseRange $ ranges !! 1}

satisfies :: Int -> Constraint -> Bool
x `satisfies` Constraint {..} = x `isInside` range1 || x `isInside` range2
