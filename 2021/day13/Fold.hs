module Fold where

import           Coordinate
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

data Fold
  = RowFold
      { rowIndex :: Int
      }
  | ColumnFold
      { columnIndex :: Int
      }

parseFold :: Text -> Fold
parseFold text =
  let parts = T.splitOn "=" text
      index = parseUnsignedInt $ parts !! 1
   in case head parts of
        "fold along x" -> ColumnFold index
        "fold along y" -> RowFold index
        _              -> error "instructions unclear, paper on fire"

apply :: Fold -> Coordinate -> Coordinate
apply RowFold {..} Coordinate {..} =
  Coordinate
    { row =
        if row <= rowIndex
          then row
          else 2 * rowIndex - row
    , column = column
    }
apply ColumnFold {..} Coordinate {..} =
  Coordinate
    { row = row
    , column =
        if column <= columnIndex
          then column
          else 2 * columnIndex - column
    }
