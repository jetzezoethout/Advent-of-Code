module CircuitContext where

import           Control.Monad       (when)
import           Control.Monad.State (State, gets, modify)
import           Data.Map.Strict     (Map, (!))
import qualified Data.Map.Strict     as M
import           Data.Maybe          (isJust)
import           Data.Sequence       (Seq, (><), (|>))
import qualified Data.Sequence       as S
import           Data.Text           (Text)
import           Module              (Module (..), Modules, processSignal)
import           Signal              (Pulse (..), Signal (..), isCycleIndicator)

data CircuitState = CircuitState
  { modules :: Modules
  , signals :: Seq Signal
  } deriving (Show)

initialState :: Modules -> CircuitState
initialState modules = CircuitState modules S.empty

type CircuitContext = State CircuitState

modifyModules :: (Modules -> Modules) -> CircuitContext ()
modifyModules f =
  modify $ \CircuitState {..} -> CircuitState (f modules) signals

modifySignals :: (Seq Signal -> Seq Signal) -> CircuitContext ()
modifySignals f =
  modify $ \CircuitState {..} -> CircuitState modules $ f signals

createButtonSignal :: CircuitContext ()
createButtonSignal = modifySignals (|> Signal Low "button" "broadcaster")

popSignal :: CircuitContext (Maybe Signal)
popSignal = do
  nextInQueue <- gets $ S.lookup 0 . signals
  when (isJust nextInQueue) $ modifySignals $ S.drop 1
  return nextInQueue

pushButton :: CircuitContext [Signal]
pushButton = createButtonSignal >> go
  where
    go :: CircuitContext [Signal]
    go = do
      nextSignal <- popSignal
      case nextSignal of
        Nothing -> return []
        Just signal@Signal {..} -> do
          nextModule <- gets $ M.lookup target . modules
          case nextModule of
            Nothing -> return ()
            Just mdl -> do
              let (updatedModule, outgoingSignals) = processSignal signal mdl
              modifyModules $ M.insert target updatedModule
              modifySignals (>< S.fromList outgoingSignals)
          (signal :) <$> go

pushButtonTimes :: Int -> CircuitContext [Signal]
pushButtonTimes n =
  if n == 0
    then return []
    else do
      observedSignals <- pushButton
      (observedSignals <>) <$> pushButtonTimes (n - 1)

findCycleTargets :: CircuitContext [Text]
findCycleTargets = do
  penultimateConjunction <-
    gets $ head . M.keys . M.filter (("rx" `elem`) . outgoing) . modules
  gets $ M.keys . memory . (! penultimateConjunction) . modules

insertWhen :: Ord k => Bool -> k -> a -> Map k a -> Map k a
insertWhen shouldInsert key value =
  if shouldInsert
    then M.insert key value
    else id

findCommonSignal :: [Text] -> CircuitContext Int
findCommonSignal cycleTargets = go 1 M.empty
  where
    go :: Int -> Map Text Int -> CircuitContext Int
    go roundNumber observedCycles = do
      observedSignals <- pushButton
      let updateCycles label =
            insertWhen
              (any (isCycleIndicator label) observedSignals)
              label
              roundNumber
          updatedCycles = foldr updateCycles observedCycles cycleTargets
      if all (`M.member` updatedCycles) cycleTargets
        then return $ foldr lcm 1 updatedCycles
        else go (roundNumber + 1) updatedCycles

findEngineStart :: CircuitContext Int
findEngineStart = findCycleTargets >>= findCommonSignal
