module RowData where

import           Cached        (Cached, runMemoized, withCache)
import           Control.Monad (join)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseUnsignedInt)
import           Record        (Record (..), dropLeadingGroup,
                                isPossiblyOperational, parseRecords, unfold)

data RowData = RowData
  { records          :: [Record]
  , contiguousGroups :: [Int]
  } deriving (Eq, Ord, Show)

parseRowData :: Text -> RowData
parseRowData text =
  let parts = T.split (== ' ') text
   in RowData
        { records = parseRecords $ head parts
        , contiguousGroups =
            map parseUnsignedInt $ T.split (== ',') $ parts !! 1
        }

getPossibilities :: RowData -> Int
getPossibilities = runMemoized . getPossibilitiesMemoized

getPossibilitiesMemoized :: RowData -> Cached RowData Int
getPossibilitiesMemoized = withCache go
  where
    go :: RowData -> Cached RowData Int
    go (RowData records [])
      | all isPossiblyOperational records = return 1
      | otherwise = return 0
    go (RowData [] _) = return 0
    go (RowData (record:records) allSizes@(size:sizes)) =
      case record of
        Operational -> getPossibilitiesMemoized $ RowData records allSizes
        Unknown -> do
          valueDamaged <-
            getPossibilitiesMemoized $ RowData (Damaged : records) allSizes
          valueOperational <-
            getPossibilitiesMemoized $ RowData (Operational : records) allSizes
          return $ valueDamaged + valueOperational
        Damaged ->
          case dropLeadingGroup (size - 1) records of
            Nothing -> return 0
            Just recordsLeft ->
              getPossibilitiesMemoized $ RowData recordsLeft sizes

unfold :: RowData -> RowData
unfold RowData {..} =
  RowData
    { records = Record.unfold 5 records
    , contiguousGroups = join $ replicate 5 contiguousGroups
    }
