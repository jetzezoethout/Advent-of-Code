module Engine where

import           Data.Maybe      (mapMaybe)
import           Data.Text       (Text)
import           EngineIndicator (EngineIndicator (..), parseEngineIndicators)
import           EngineNumber    (EngineNumber, isAdjacentTo,
                                  parseEngineNumbers)
import           Gear            (Gear, getGear)
import           TaggedRow       (TaggedRow, parseTaggedLines)

data Engine = Engine
  { numbers    :: [EngineNumber]
  , indicators :: [EngineIndicator]
  } deriving (Show)

parseEngineLines :: [TaggedRow] -> Engine
parseEngineLines taggedLines =
  Engine
    { numbers = taggedLines >>= parseEngineNumbers
    , indicators = taggedLines >>= parseEngineIndicators
    }

parseEngine :: Text -> Engine
parseEngine = parseEngineLines . parseTaggedLines

isEnginePartNumber :: [EngineIndicator] -> EngineNumber -> Bool
isEnginePartNumber indicators number =
  any (\ind -> ind.location `isAdjacentTo` number) indicators

getEnginePartNumbers :: Engine -> [EngineNumber]
getEnginePartNumbers Engine {..} =
  filter (isEnginePartNumber indicators) numbers

getGears :: Engine -> [Gear]
getGears Engine {..} = mapMaybe (getGear numbers) indicators
