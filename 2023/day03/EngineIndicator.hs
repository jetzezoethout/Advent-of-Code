module EngineIndicator where

import           Coordinate  (Coordinate (..))
import           Data.Char   (isDigit)
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import           LocatedChar (LocatedChar (..), locateText)

data EngineIndicator = EngineIndicator
  { location :: Coordinate
  , symbol   :: Char
  } deriving (Show)

isIndicatorSymbol :: Char -> Bool
isIndicatorSymbol ch = ch /= '.' && not (isDigit ch)

parseEngineIndicators :: Text -> [EngineIndicator]
parseEngineIndicators = mapMaybe findIndicator . locateText
  where
    findIndicator LocatedChar {..} =
      [ EngineIndicator {location = location, symbol = char}
      | isIndicatorSymbol char
      ]
