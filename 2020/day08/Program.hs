module Program where

import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Instruction (Instruction (..), parseInstruction,
                              repairInstruction, updateAccumulator,
                              updateAddress)

newtype Program = Program
  { instructions :: Vector Instruction
  }

parseProgram :: Text -> Program
parseProgram = Program . V.fromList . map parseInstruction . T.lines

data ProgramState = ProgramState
  { currentAddress :: Int
  , accumulator    :: Int
  , seenAddresses  :: Set Int
  }

initialState :: ProgramState
initialState =
  ProgramState {currentAddress = 0, accumulator = 0, seenAddresses = S.empty}

runUntilInfiniteLoop :: Program -> Int
runUntilInfiniteLoop program = go initialState
  where
    go state =
      case nextInstruction program state of
        Left InfiniteLoop -> state.accumulator
        Left _            -> error "no infinite loop detected"
        Right instruction -> go $ processInstruction state instruction

repairProgram :: Program -> Int
repairProgram program = go initialState
  where
    go :: ProgramState -> Int
    go state =
      case nextInstruction program state of
        Left _ -> error "program damaged beyond repair"
        Right instruction ->
          case repairInstruction instruction
                 >>= tryRepairedProgram . processInstruction state of
            Nothing     -> go $ processInstruction state instruction
            Just result -> result
    tryRepairedProgram :: ProgramState -> Maybe Int
    tryRepairedProgram state =
      case nextInstruction program state of
        Left Terminated -> Just state.accumulator
        Left _ -> Nothing
        Right instruction ->
          tryRepairedProgram $ processInstruction state instruction

processInstruction :: ProgramState -> Instruction -> ProgramState
processInstruction ProgramState {..} instruction =
  ProgramState
    { currentAddress = updateAddress instruction currentAddress
    , accumulator = updateAccumulator instruction accumulator
    , seenAddresses = S.insert currentAddress seenAddresses
    }

data ExitCode
  = Terminated
  | InfiniteLoop
  | Crashed

nextInstruction :: Program -> ProgramState -> Either ExitCode Instruction
nextInstruction Program {..} ProgramState {..}
  | currentAddress < 0 = Left Crashed
  | currentAddress > V.length instructions = Left Crashed
  | currentAddress == V.length instructions = Left Terminated
  | currentAddress `S.member` seenAddresses = Left InfiniteLoop
  | otherwise = Right $ instructions ! currentAddress
