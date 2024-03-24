module History where

import           Control.Monad (guard)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseInt)

newtype History = History
  { getHistory :: [Int]
  } deriving (Show)

parseHistory :: Text -> History
parseHistory = History . map parseInt . T.words

reverseHistory :: History -> History
reverseHistory = History . reverse . getHistory

derivative :: History -> History
derivative History {..} = History $ zipWith (-) (tail getHistory) getHistory

hasNonZeroValues :: History -> Bool
hasNonZeroValues History {..} = any (/= 0) getHistory

deriveUntilZero :: History -> [History]
deriveUntilZero history =
  history
    : (guard (hasNonZeroValues history) >> deriveUntilZero (derivative history))

extrapolate :: History -> Int
extrapolate = sum . map (last . getHistory) . deriveUntilZero
