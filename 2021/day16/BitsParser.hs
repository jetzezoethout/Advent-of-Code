module BitsParser where

import           Bit                 (Bit (..), toInt)
import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.State (StateT (StateT), evalStateT, gets,
                                      replicateM)
import           Data.Maybe          (fromJust)
import           Packet              (Packet (..), toOperation)

data ParseState = ParseState
  { bitsRead      :: Int
  , remainingBits :: [Bit]
  }

type BitsParser = StateT ParseState Maybe

readBit :: BitsParser Bit
readBit =
  StateT $ \ParseState {..} ->
    case remainingBits of
      []       -> Nothing
      bit:bits -> Just (bit, ParseState (bitsRead + 1) bits)

parsePacket :: [Bit] -> Packet
parsePacket = fromJust . evalStateT packet . ParseState 0

packet :: BitsParser Packet
packet = do
  version <- toInt <$> replicateM 3 readBit
  typeId <- toInt <$> replicateM 3 readBit
  case typeId of
    4 -> literalPacket version
    _ -> operatorPacket version typeId

literalPacket :: Int -> BitsParser Packet
literalPacket version = LiteralPacket version . toInt <$> go
  where
    go = do
      signalBit <- readBit
      nextChunk <- replicateM 4 readBit
      case signalBit of
        Zero -> return nextChunk
        One  -> (nextChunk <>) <$> go

operatorPacket :: Int -> Int -> BitsParser Packet
operatorPacket version typeId =
  OperatorPacket version (toOperation typeId) <$> go
  where
    go = do
      lengthTypeId <- readBit
      case lengthTypeId of
        Zero -> subPacketsByLength
        One  -> subPacketsByCount

subPacketsByLength :: BitsParser [Packet]
subPacketsByLength = do
  bitsToRead <- toInt <$> replicateM 15 readBit
  marker <- gets bitsRead
  go (marker + bitsToRead)
  where
    go goal = do
      currentPosition <- gets bitsRead
      if currentPosition >= goal
        then return []
        else liftA2 (:) packet $ go goal

subPacketsByCount :: BitsParser [Packet]
subPacketsByCount = do
  count <- toInt <$> replicateM 11 readBit
  go count
  where
    go 0 = return []
    go n = liftA2 (:) packet $ go $ n - 1
