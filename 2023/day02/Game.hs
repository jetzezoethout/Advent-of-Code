module Game where

import           CubeSample (CubeSample, isBoundedBy, parseSamples)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

data Game = Game
  { gameId  :: Int
  , samples :: [CubeSample]
  } deriving (Show)

parseGameId :: Text -> Int
parseGameId text = parseUnsignedInt $ T.split (== ' ') text !! 1

parseGame :: Text -> Game
parseGame text =
  let parts = T.split (== ':') text
      gameId = parseGameId $ head parts
      samples = parseSamples $ T.strip (parts !! 1)
   in Game gameId samples

isPossibleGiven :: Game -> CubeSample -> Bool
Game {..} `isPossibleGiven` bound = all (`isBoundedBy` bound) samples
