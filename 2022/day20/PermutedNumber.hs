module PermutedNumber where

import           Data.Maybe    (fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseInt)

data PermutedNumber = PermutedNumber
  { originalIndex :: Int
  , value         :: Int
  } deriving (Show)

parseEncryptedFile :: Text -> Seq PermutedNumber
parseEncryptedFile =
  S.fromList . zipWith PermutedNumber [0 ..] . map parseInt . T.lines

move :: Int -> Seq PermutedNumber -> Seq PermutedNumber
move index numbers =
  S.insertAt insertionIndex itemToMove $ S.deleteAt currentIndex numbers
  where
    currentIndex = fromJust $ S.findIndexL ((== index) . originalIndex) numbers
    itemToMove = numbers `S.index` currentIndex
    insertionIndex =
      (currentIndex + value itemToMove) `mod` (S.length numbers - 1)

decrypt :: Seq PermutedNumber -> Seq PermutedNumber
decrypt numbers =
  foldl (flip ($)) numbers $ map move [0 .. S.length numbers - 1]

groveCoordinates :: Seq PermutedNumber -> [Int]
groveCoordinates numbers =
  let zeroIndex = fromJust $ S.findIndexL ((== 0) . value) numbers
   in map
        (\i -> value $ numbers `S.index` ((zeroIndex + i) `mod` length numbers))
        [1000, 2000, 3000]

keyd :: PermutedNumber -> PermutedNumber
keyd PermutedNumber {..} =
  PermutedNumber {originalIndex = originalIndex, value = value * 811589153}
