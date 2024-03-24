module Pattern where

import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GroundElement   (GroundElement, fromChar)
import           MirrorData      (MirrorData (..), Orientation (..),
                                  transposeMirror)
import           MirrorEq        (MirrorEq (mirrorEquals), MirrorEquality (..))

newtype Pattern = Pattern
  { rows :: [[GroundElement]]
  } deriving (MirrorEq)

parsePattern :: [Text] -> Pattern
parsePattern = Pattern . map (map fromChar . T.unpack)

parsePatterns :: Text -> [Pattern]
parsePatterns = map parsePattern . splitOn [""] . T.lines

transposePattern :: Pattern -> Pattern
transposePattern (Pattern {..}) = Pattern $ transpose rows

getMirroredPairs :: Int -> Int -> [(Int, Int)]
getMirroredPairs mirrorPosition numberOfRows =
  zip (reverse [0 .. mirrorPosition - 1]) [mirrorPosition .. numberOfRows - 1]

findHorizontalMirrorsBy :: MirrorEquality -> Pattern -> [MirrorData]
findHorizontalMirrorsBy target Pattern {..} =
  map (MirrorData Horizontal) $ filter hasMirrorAt [1 .. numberOfRows - 1]
  where
    numberOfRows = length rows
    hasMirrorAt position =
      let leftHandSide = reverse $ take position rows
          rightHandside = drop position rows
       in leftHandSide `mirrorEquals` rightHandside == target

findVerticalMirrorsBy :: MirrorEquality -> Pattern -> [MirrorData]
findVerticalMirrorsBy target =
  map transposeMirror . findHorizontalMirrorsBy target . transposePattern

findMirrorsBy :: MirrorEquality -> Pattern -> [MirrorData]
findMirrorsBy target pattern =
  findHorizontalMirrorsBy target pattern <> findVerticalMirrorsBy target pattern
