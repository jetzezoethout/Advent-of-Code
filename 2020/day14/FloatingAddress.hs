module FloatingAddress where

import           Bit        (BinaryNumber, Bit (..), BitMask)
import           FixedWidth (FixedWidth, branchFixedWidth, zipFixedWidth)

data FloatingBit
  = Floating
  | Determined Bit
  deriving (Show)

possibleBits :: FloatingBit -> [Bit]
possibleBits Floating         = [Zero, One]
possibleBits (Determined bit) = [bit]

type FloatingAddress = FixedWidth FloatingBit

toFloatingAddress :: BinaryNumber -> FloatingAddress
toFloatingAddress = fmap Determined

maskFloatingAddress :: BitMask -> FloatingAddress -> FloatingAddress
maskFloatingAddress = zipFixedWidth merge
  where
    merge (Just Zero) floatingBit = floatingBit
    merge (Just One) _            = Determined One
    merge Nothing _               = Floating

generateAddresses :: FloatingAddress -> [BinaryNumber]
generateAddresses = branchFixedWidth possibleBits
