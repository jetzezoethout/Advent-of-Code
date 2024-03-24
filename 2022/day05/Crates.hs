module Crates where

import           CraneType       (CraneType, Crate, reStack)
import           Data.Char       (digitToInt, isSpace)
import           Data.IntMap     (IntMap, (!))
import qualified Data.IntMap     as M
import           Data.List       (foldl', transpose)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           MoveInstruction (MoveInstruction (..))

type Crates = IntMap [Crate]

parseCrates :: [Text] -> Crates
parseCrates = M.fromList . map parseCrate . transpose . map horizontalSlice
  where
    horizontalSlice :: Text -> [Char]
    horizontalSlice = map (`T.index` 1) . T.chunksOf 4
    parseCrate :: [Char] -> (Int, [Char])
    parseCrate chs = (digitToInt $ last chs, dropWhile isSpace $ init chs)

topCrates :: Crates -> [Crate]
topCrates crates = map head $ M.elems crates

processInstruction :: CraneType -> Crates -> MoveInstruction -> Crates
processInstruction craneType crates MoveInstruction {..} =
  let (toMove, leftBehind) = splitAt amount $ crates ! source
   in M.insert source leftBehind
        $ M.adjust (reStack craneType toMove <>) target crates

processInstructions :: CraneType -> Crates -> [MoveInstruction] -> Crates
processInstructions = foldl' . processInstruction
