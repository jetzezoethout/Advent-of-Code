module Bus where

import           Data.Maybe (catMaybes)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

data Bus = Bus
  { ordinal :: Int
  , busId   :: Int
  } deriving (Show)

parseBusId :: Text -> Maybe Int
parseBusId "x"   = Nothing
parseBusId busId = Just $ parseUnsignedInt busId

parseBusses :: Text -> [Bus]
parseBusses =
  catMaybes
    . zipWith (\ordinal -> (Bus ordinal <$>)) [0 ..]
    . map parseBusId
    . T.splitOn ","

nextDepartureFrom :: Int -> Bus -> Int
nextDepartureFrom now Bus {..} = (-now) `mod` busId
