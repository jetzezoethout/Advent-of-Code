module Game where

import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Deck            (Deck, Play (..), createSubDeck,
                                  insertOnBottom, parseDeck, play)

data Result = Result
  { firstPlayerWins :: Bool
  , winningDeck     :: Deck
  } deriving (Show)

data Game = Game
  { deck1 :: Deck
  , deck2 :: Deck
  } deriving (Show, Eq, Ord)

parseGame :: Text -> Game
parseGame text =
  let deckInfos = splitOn [""] $ T.lines text
      deck1 = parseDeck $ head deckInfos
      deck2 = parseDeck $ deckInfos !! 1
   in Game {..}

type Beats = Play -> Play -> Bool

playRound :: Beats -> Game -> Either Result Game
playRound beats Game {..} =
  case (play deck1, play deck2) of
    (Nothing, _) -> Left $ Result {firstPlayerWins = False, winningDeck = deck2}
    (_, Nothing) -> Left $ Result {firstPlayerWins = True, winningDeck = deck1}
    (Just play1, Just play2) ->
      Right
        $ if play1 `beats` play2
            then Game
                   { deck1 =
                       insertOnBottom
                         play1.left
                         [play1.cardPlayed, play2.cardPlayed]
                   , deck2 = play2.left
                   }
            else Game
                   { deck1 = play1.left
                   , deck2 =
                       insertOnBottom
                         play2.left
                         [play2.cardPlayed, play1.cardPlayed]
                   }

playNormalRound :: Game -> Either Result Game
playNormalRound = playRound beats
  where
    beats :: Beats
    play1 `beats` play2 = play1.cardPlayed > play2.cardPlayed

playNormal :: Game -> Result
playNormal = go
  where
    go game =
      case playNormalRound game of
        Left result    -> result
        Right nextGame -> playNormal nextGame

playRecursiveRound :: Set Game -> Game -> Either Result Game
playRecursiveRound previousStates game =
  if game `S.member` previousStates
    then cutShort game
    else playRound beats game
  where
    beats :: Beats
    play1 `beats` play2 =
      case (createSubDeck play1, createSubDeck play2) of
        (Just subDeck1, Just subDeck2) ->
          firstPlayerWins $ recurse subDeck1 subDeck2
        _ -> play1.cardPlayed > play2.cardPlayed
    cutShort Game {..} =
      Left $ Result {firstPlayerWins = True, winningDeck = deck1}
    recurse subDeck1 subDeck2 =
      playRecursive $ Game {deck1 = subDeck1, deck2 = subDeck2}

playRecursive :: Game -> Result
playRecursive = go S.empty
  where
    go previousStates game =
      case playRecursiveRound previousStates game of
        Left result    -> result
        Right nextGame -> go (game `S.insert` previousStates) nextGame
