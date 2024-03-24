module EngineIndicator where

import           Coordinate (Coordinate (..))
import           Data.Char  (isDigit)
import qualified Data.Text  as T
import           TaggedRow  (TaggedRow (..))

data EngineIndicator = EngineIndicator
  { location :: Coordinate
  , symbol   :: Char
  } deriving (Show)

isIndicatorSymbol :: Char -> Bool
isIndicatorSymbol ch = ch /= '.' && not (isDigit ch)

parseEngineIndicators :: TaggedRow -> [EngineIndicator]
parseEngineIndicators TaggedRow {..} =
  filter (isIndicatorSymbol . symbol)
    $ map
        (\i ->
           EngineIndicator
             { location = Coordinate {row = rowIndex, column = i}
             , symbol = T.index content i
             })
        [0 .. (T.length content - 1)]
