module Module where

import           Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as M
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Signal          (Pulse (..), Signal (..))

data FlipFlopState
  = Off
  | On
  deriving (Eq, Show)

toggle :: FlipFlopState -> FlipFlopState
toggle Off = On
toggle On  = Off

getPulse :: FlipFlopState -> Pulse
getPulse Off = Low
getPulse On  = High

data Module
  = Broadcaster
      { outgoing :: [Text]
      }
  | FlipFlop
      { outgoing :: [Text]
      , state    :: FlipFlopState
      }
  | Conjunction
      { outgoing :: [Text]
      , memory   :: Map Text Pulse
      }
  deriving (Show)

type Modules = Map Text Module

parseModule :: Text -> (Text, Module)
parseModule text =
  let parts = T.splitOn " -> " text
      header = head parts
      outgoing = T.splitOn ", " $ parts !! 1
   in case T.head header of
        '%' -> (T.tail header, FlipFlop {outgoing = outgoing, state = Off})
        '&' ->
          (T.tail header, Conjunction {outgoing = outgoing, memory = M.empty})
        _ -> (header, Broadcaster {outgoing = outgoing})

parseModules :: Text -> Modules
parseModules text =
  let pairs = map parseModule $ T.lines text
   in foldr connectIncomings (fromList pairs) pairs

connectIncomings :: (Text, Module) -> Modules -> Modules
connectIncomings (label, Conjunction outs _) modules =
  let incomingModules = M.keys $ M.filter ((label `elem`) . outgoing) modules
      connectedModule =
        Conjunction outs $ fromList $ map (, Low) incomingModules
   in M.insert label connectedModule modules
connectIncomings _ modules = modules

processSignal :: Signal -> Module -> (Module, [Signal])
processSignal Signal {..} mdl@Broadcaster {..} =
  (mdl, map (Signal pulse target) outgoing)
processSignal Signal {..} mdl@FlipFlop {..} =
  case pulse of
    High -> (mdl, [])
    Low ->
      let updatedState = toggle state
       in ( FlipFlop outgoing updatedState
          , map (Signal (getPulse updatedState) target) outgoing)
processSignal Signal {..} Conjunction {..} =
  let updatedMemory = M.insert source pulse memory
      outgoingPulse =
        if all (== High) updatedMemory
          then Low
          else High
   in ( Conjunction outgoing updatedMemory
      , map (Signal outgoingPulse target) outgoing)
