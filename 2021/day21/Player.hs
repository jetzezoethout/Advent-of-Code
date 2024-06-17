module Player where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Player
  = Player1
  | Player2
  deriving (Eq, Ord, Show)

opponent :: Player -> Player
opponent Player1 = Player2
opponent Player2 = Player1

data PlayerState = PlayerState
  { position :: Int
  , score    :: Int
  } deriving (Show, Eq, Ord)

parsePlayerState :: Text -> PlayerState
parsePlayerState = initialState . parseUnsignedInt . last . T.words

initialState :: Int -> PlayerState
initialState initialPosition = PlayerState initialPosition 0

move :: Int -> PlayerState -> PlayerState
move amount PlayerState {..} =
  let newPosition = fitOnBoard $ position + amount
   in PlayerState {position = newPosition, score = score + newPosition}

fitOnBoard :: Int -> Int
fitOnBoard n = ((n - 1) `mod` 10) + 1
