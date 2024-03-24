module Parsers
  ( parseUnsignedIntegral
  , parseUnsignedInt
  , parseIntegral
  , parseInt
  , parseHex
  ) where

import           Data.Either    (fromRight)
import           Data.Text      (Text, unpack)
import           Data.Text.Read (decimal, hexadecimal, signed)

parseUnsignedIntegral :: Integral a => Text -> a
parseUnsignedIntegral text =
  fst $ fromRight (error $ unpack text <> " is not a number") $ decimal text

parseUnsignedInt :: Text -> Int
parseUnsignedInt = parseUnsignedIntegral @Int

parseIntegral :: Integral a => Text -> a
parseIntegral text =
  fst
    $ fromRight (error $ unpack text <> " is not a number")
    $ signed decimal text

parseInt :: Text -> Int
parseInt = parseIntegral @Int

parseHex :: Text -> Int
parseHex text =
  fst
    $ fromRight (error $ unpack text <> " is not a hex number")
    $ hexadecimal text
