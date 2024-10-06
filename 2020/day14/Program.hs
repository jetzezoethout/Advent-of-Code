module Program where

import           Bit       (BinaryNumber, BitMask, parseBitMask, toBinary)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Instruction
  = SetMask
      { newMask :: BitMask
      }
  | WriteMemory
      { address :: BinaryNumber
      , value   :: BinaryNumber
      }
  deriving (Show)

type Program = [Instruction]

parseInstruction :: Text -> Instruction
parseInstruction text =
  let parts = T.splitOn " = " text
   in if head parts == "mask"
        then SetMask {newMask = parseBitMask $ parts !! 1}
        else WriteMemory
               { address =
                   toBinary $ parseUnsignedInt $ T.init $ T.drop 4 $ head parts
               , value = toBinary $ parseUnsignedInt $ parts !! 1
               }

parseProgram :: Text -> [Instruction]
parseProgram = map parseInstruction . T.lines
