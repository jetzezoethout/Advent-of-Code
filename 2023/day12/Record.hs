module Record where

import           Control.Monad (guard)

import           Data.Text     (Text)
import qualified Data.Text     as T

data Record
  = Operational
  | Damaged
  | Unknown
  deriving (Eq, Ord, Show)

parseRecords :: Text -> [Record]
parseRecords = map fromChar . T.unpack

fromChar :: Char -> Record
fromChar '.' = Operational
fromChar '#' = Damaged
fromChar '?' = Unknown
fromChar _   = error "not a record"

isPossiblyDamaged :: Record -> Bool
isPossiblyDamaged = (/= Operational)

isPossiblyOperational :: Record -> Bool
isPossiblyOperational = (/= Damaged)

dropLeadingGroup :: Int -> [Record] -> Maybe [Record]
dropLeadingGroup 0 [] = Just []
dropLeadingGroup 0 (r:rs) = guard (isPossiblyOperational r) >> Just rs
dropLeadingGroup _ [] = Nothing
dropLeadingGroup n (r:rs) =
  guard (isPossiblyDamaged r) >> dropLeadingGroup (n - 1) rs

unfold :: Int -> [Record] -> [Record]
unfold times records =
  case times of
    1 -> records
    n -> records <> [Unknown] <> unfold (n - 1) records
