module DisplaySolver where

import           Data.List  (find, intersect)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Segment    (Segment (..), allSegments, fromChar)

data DisplaySolver = DisplaySolver
  { one             :: [Segment]
  , seven           :: [Segment]
  , four            :: [Segment]
  , zeroSixNineGaps :: [Segment]
  }

parseDisplaySolver :: Text -> DisplaySolver
parseDisplaySolver text =
  DisplaySolver
    { one = map fromChar $ T.unpack oneRep
    , seven = map fromChar $ T.unpack sevenRep
    , four = map fromChar $ T.unpack fourRep
    , zeroSixNineGaps = map findMissing zeroSixNineReps
    }
  where
    parts = T.words text
    oneRep = fromJust $ find ((== 2) . T.length) parts
    sevenRep = fromJust $ find ((== 3) . T.length) parts
    fourRep = fromJust $ find ((== 4) . T.length) parts
    zeroSixNineReps = filter ((== 6) . T.length) parts
    findMissing zeroSixOrNine =
      fromJust
        $ find (`notElem` map fromChar (T.unpack zeroSixOrNine)) allSegments

findSegments :: DisplaySolver -> Segment -> Segment
findSegments DisplaySolver {..} segment =
  case segment of
    A -> fromJust $ find (`notElem` one) seven
    B -> fromJust $ find (`notElem` one <> zeroSixNineGaps) four
    C -> head $ one `intersect` zeroSixNineGaps
    D -> fromJust $ find (`notElem` one) $ four `intersect` zeroSixNineGaps
    E -> fromJust $ find (`notElem` four) zeroSixNineGaps
    F -> fromJust $ find (`notElem` zeroSixNineGaps) one
    G ->
      fromJust $ find (`notElem` seven <> four <> zeroSixNineGaps) allSegments

mapBackwards :: DisplaySolver -> Map Segment Segment
mapBackwards solver = M.fromList [(findSegments solver s, s) | s <- allSegments]

decrypt :: Text -> Map Segment Segment
decrypt = mapBackwards . parseDisplaySolver
