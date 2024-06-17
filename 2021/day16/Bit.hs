module Bit where

import           Control.Monad ((>=>))
import           Data.List     (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseHex)

data Bit
  = Zero
  | One
  deriving (Show)

fromInt :: Int -> [Bit]
fromInt = go [] 4
  where
    go :: [Bit] -> Int -> Int -> [Bit]
    go acc 0 _ = acc
    go acc toGo n =
      let nextBit =
            if even n
              then Zero
              else One
       in go (nextBit : acc) (toGo - 1) (n `div` 2)

fromHex :: Char -> [Bit]
fromHex = fromInt . parseHex . T.singleton

toInt :: [Bit] -> Int
toInt = foldl' (\acc bit -> 2 * acc + bitToInt bit) 0
  where
    bitToInt Zero = 0
    bitToInt One  = 1

parseBits :: Text -> [Bit]
parseBits = T.unpack >=> fromHex
