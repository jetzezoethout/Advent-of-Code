module Instruction
  ( hash
  , Instruction(..)
  , parseInstruction
  ) where

import           Box       (Box, add, remove)
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

hashStep :: Int -> Int -> Int
hashStep current nextValue = ((current + nextValue) * 17) `mod` 256

hash :: Text -> Int
hash = foldl' hashStep 0 . map fromEnum . T.unpack

data Instruction = Instruction
  { boxNumber :: Int
  , operation :: Box -> Box
  }

parseInstruction :: Text -> Instruction
parseInstruction text =
  if length parts == 1
    then let label = T.init $ head parts
          in Instruction {boxNumber = hash label, operation = remove label}
    else let label = head parts
          in Instruction
               { boxNumber = hash label
               , operation = add label $ parseUnsignedInt $ parts !! 1
               }
  where
    parts = T.split (== '=') text
