module MoveInstruction where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data MoveInstruction = MoveInstruction
  { amount :: Int
  , source :: Int
  , target :: Int
  } deriving (Show)

parseMoveInstruction :: Text -> MoveInstruction
parseMoveInstruction text =
  let parts = T.words text
   in MoveInstruction
        { amount = parseUnsignedInt $ parts !! 1
        , source = parseUnsignedInt $ parts !! 3
        , target = parseUnsignedInt $ parts !! 5
        }
