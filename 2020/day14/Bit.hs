module Bit where

import           Data.List  (foldl')
import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           FixedWidth (FixedWidth (contents), fixedWidth, leftPad,
                             zipFixedWidth)

data Bit
  = Zero
  | One
  deriving (Show, Eq, Ord)

fromBit :: Bit -> Int
fromBit Zero = 0
fromBit One  = 1

toBit :: Int -> Bit
toBit 0 = Zero
toBit 1 = One
toBit _ = error "not a bit"

type BinaryNumber = FixedWidth Bit

toBinary :: Int -> BinaryNumber
toBinary = leftPad Zero . go []
  where
    go :: [Bit] -> Int -> [Bit]
    go acc 0 = acc
    go acc n = go (toBit (n `mod` 2) : acc) (n `div` 2)

fromBinary :: BinaryNumber -> Int
fromBinary = foldl' (\acc bit -> 2 * acc + fromBit bit) 0 . contents

type BitMask = FixedWidth (Maybe Bit)

emptyMask :: BitMask
emptyMask = leftPad Nothing []

parseBitMask :: Text -> BitMask
parseBitMask = fixedWidth . map fromChar . T.unpack
  where
    fromChar 'X' = Nothing
    fromChar '0' = Just Zero
    fromChar '1' = Just One
    fromChar _   = error "invalid mask element"

applyMask :: BitMask -> BinaryNumber -> BinaryNumber
applyMask = zipFixedWidth (flip fromMaybe)
