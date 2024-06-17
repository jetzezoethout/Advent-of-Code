module Game where

import           Cached      (Cached, runMemoized, withCache)
import           Data.Monoid (Sum (..))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Player      (Player (..), PlayerState (..), move, opponent,
                              parsePlayerState)

data Game = Game
  { currentPlayer      :: Player
  , currentPlayerState :: PlayerState
  , opponentState      :: PlayerState
  } deriving (Show, Eq, Ord)

parseGame :: Text -> Game
parseGame text =
  let textLines = T.lines text
   in Game
        { currentPlayer = Player1
        , currentPlayerState = parsePlayerState $ head textLines
        , opponentState = parsePlayerState $ textLines !! 1
        }

playOnce :: Game -> Int -> Game
playOnce Game {..} amount =
  Game
    { currentPlayer = opponent currentPlayer
    , currentPlayerState = opponentState
    , opponentState = move amount currentPlayerState
    }

playDeterminstic :: Game -> Int
playDeterminstic = go 0
  where
    go :: Int -> Game -> Int
    go timesThrown game@Game {..} =
      if score opponentState >= 1000
        then score currentPlayerState * timesThrown
        else go (timesThrown + 3) $ playOnce game $ 3 * timesThrown + 6

playDirac :: Game -> Int
playDirac initialGame = max playerOneWins playerTwoWins
  where
    (Sum playerOneWins, Sum playerTwoWins) = runMemoized $ go initialGame
    go :: Game -> Cached Game (Sum Int, Sum Int)
    go game@Game {..} =
      if score opponentState >= 21
        then return
               $ case currentPlayer of
                   Player1 -> (Sum 0, Sum 1)
                   Player2 -> (Sum 1, Sum 0)
        else withCache splitPossibilities game
    splitPossibilities :: Game -> Cached Game (Sum Int, Sum Int)
    splitPossibilities game =
      mconcat <$> traverse (go . playOnce game) possibleThrows
    possibleThrows :: [Int]
    possibleThrows =
      [x + y + z | x <- [1, 2, 3], y <- [1, 2, 3], z <- [1, 2, 3]]
