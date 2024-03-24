module Round where

import           Data.Text (Text)
import qualified Data.Text as T
import           Move      (Move, against, moveScore, parseMove, versus)
import           Outcome   (Outcome, outcomeScore, parseOutcome)

data Round = Round
  { ours    :: Move
  , theirs  :: Move
  , outcome :: Outcome
  }

roundScore :: Round -> Int
roundScore Round {..} = moveScore ours + outcomeScore outcome

fromMoves :: Move -> Move -> Round
fromMoves ours theirs = Round ours theirs (ours `versus` theirs)

parseFromMoves :: Text -> Round
parseFromMoves text =
  let characters = T.unpack text
      oursData = characters !! 2
      theirsData = head characters
   in fromMoves (parseMove oursData) (parseMove theirsData)

fromGoal :: Outcome -> Move -> Round
fromGoal goal theirs = Round (goal `against` theirs) theirs goal

parseFromGoal :: Text -> Round
parseFromGoal text =
  let characters = T.unpack text
      goalData = characters !! 2
      theirsData = head characters
   in fromGoal (parseOutcome goalData) (parseMove theirsData)
