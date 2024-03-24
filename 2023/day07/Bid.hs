module Bid where

import           Data.Function (on)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Hand          (Hand, parseHand)
import           JokerOrd      (JokerOrd (jokerCompare))
import           Parsers       (parseUnsignedInt)

data Bid = Bid
  { hand :: Hand
  , bid  :: Int
  } deriving (Eq, Show)

instance Ord Bid where
  compare :: Bid -> Bid -> Ordering
  compare = compare `on` hand

parseBid :: Text -> Bid
parseBid text = Bid (parseHand $ head parts) (parseUnsignedInt $ parts !! 1)
  where
    parts = T.words text

rankedScore :: Int -> Bid -> Int
rankedScore rank Bid {..} = rank * bid

instance JokerOrd Bid where
  jokerCompare :: Bid -> Bid -> Ordering
  jokerCompare = jokerCompare `on` hand
