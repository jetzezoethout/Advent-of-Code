module KeepAwayGame where

import           Control.Monad        (replicateM_)
import           Control.Monad.Reader (MonadReader (reader),
                                       ReaderT (runReaderT))
import           Control.Monad.State  (State, execState, gets, modify)
import           Data.Foldable        (traverse_)
import           Data.IntMap          (IntMap, (!))
import qualified Data.IntMap          as M
import qualified Data.Sequence        as S
import           InspectionRule       (InspectionRule (..))
import           MonkeyState          (MonkeyState (..), handleItem,
                                       receiveItem)

type KeepAwayGame = ReaderT (IntMap InspectionRule) (State (IntMap MonkeyState))

toMap :: [a] -> IntMap a
toMap = M.fromList . zip [0 ..]

runGame :: KeepAwayGame () -> [InspectionRule] -> [MonkeyState] -> [MonkeyState]
runGame game rules initialState =
  M.elems $ execState (runReaderT game $ toMap rules) $ toMap initialState

getRule :: Int -> KeepAwayGame InspectionRule
getRule monkey = reader (! monkey)

getMonkey :: Int -> KeepAwayGame MonkeyState
getMonkey monkey = gets (! monkey)

processMonkey :: Int -> KeepAwayGame ()
processMonkey monkey = do
  currentState <- getMonkey monkey
  case S.lookup 0 currentState.items of
    Nothing -> return ()
    Just nextItem -> do
      modify $ M.adjust handleItem monkey
      rule <- getRule monkey
      let updatedWorryLevel = rule.operation nextItem
          receivingMonkey = rule.getNextMonkey updatedWorryLevel
      modify $ M.adjust (receiveItem updatedWorryLevel) receivingMonkey
      processMonkey monkey

monkeyRound :: KeepAwayGame ()
monkeyRound = do
  monkeys <- gets M.keys
  traverse_ processMonkey monkeys

monkeyRounds :: Int -> KeepAwayGame ()
monkeyRounds n = replicateM_ n monkeyRound
