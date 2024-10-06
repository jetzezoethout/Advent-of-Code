module DecoderChip where

import           Bit             (BinaryNumber, BitMask, applyMask, emptyMask,
                                  fromBinary)
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as M
import           Data.List       (foldl')
import           FloatingAddress (generateAddresses, maskFloatingAddress,
                                  toFloatingAddress)
import           Program         (Instruction (..), Program)

data DecoderChip = Machine
  { mask   :: BitMask
  , memory :: IntMap BinaryNumber
  } deriving (Show)

newMachine :: DecoderChip
newMachine = Machine {mask = emptyMask, memory = M.empty}

totalMemory :: DecoderChip -> Int
totalMemory Machine {..} = sum $ map fromBinary $ M.elems memory

applyInstruction :: DecoderChip -> Instruction -> DecoderChip
applyInstruction machine SetMask {..} = machine {mask = newMask}
applyInstruction Machine {..} WriteMemory {..} =
  Machine
    { mask = mask
    , memory = M.insert (fromBinary address) (applyMask mask value) memory
    }

execute :: DecoderChip -> Program -> DecoderChip
execute = foldl' applyInstruction

applyInstructionV2 :: DecoderChip -> Instruction -> DecoderChip
applyInstructionV2 machine SetMask {..} = machine {mask = newMask}
applyInstructionV2 Machine {..} WriteMemory {..} =
  Machine
    { mask = mask
    , memory =
        foldl' (\acc addr -> M.insert addr value acc) memory
          $ map fromBinary
          $ generateAddresses
          $ maskFloatingAddress mask
          $ toFloatingAddress address
    }

executeV2 :: DecoderChip -> Program -> DecoderChip
executeV2 = foldl' applyInstructionV2
