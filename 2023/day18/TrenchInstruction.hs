module TrenchInstruction where

import           Data.Text (Text)
import qualified Data.Text as T
import           Direction (Direction (..))
import           Parsers   (parseHex, parseUnsignedInt)

data TrenchInstruction = TrenchInstruction
  { trenchDirection :: Direction
  , trenchLength    :: Int
  } deriving (Show)

parseDir :: Text -> Direction
parseDir "U" = North
parseDir "R" = East
parseDir "D" = South
parseDir "L" = West
parseDir _   = error "invalid direction"

parseTrenchInstructions :: Text -> (TrenchInstruction, TrenchInstruction)
parseTrenchInstructions text =
  ( TrenchInstruction
      { trenchDirection = parseDir $ head parts
      , trenchLength = parseUnsignedInt $ parts !! 1
      }
  , parseHexInstruction $ parts !! 2)
  where
    parts = T.words text

fromDigit :: Char -> Direction
fromDigit '0' = East
fromDigit '1' = South
fromDigit '2' = West
fromDigit '3' = North
fromDigit _   = error "invalid direction ordinal"

parseHexInstruction :: Text -> TrenchInstruction
parseHexInstruction text =
  TrenchInstruction
    { trenchDirection = fromDigit directionPart
    , trenchLength = parseHex lengthPart
    }
  where
    properContent = T.drop 2 $ T.init text
    lengthPart = T.init properContent
    directionPart = T.last properContent
