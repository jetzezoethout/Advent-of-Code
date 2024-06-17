module TransparentPaper where

import           Coordinate (Coordinate (..))
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Fold       (Fold, apply)
import           Parsers    (parseUnsignedInt)

type TransparentPaper = Set Coordinate

parseTransparentPaper :: [Text] -> TransparentPaper
parseTransparentPaper = S.fromList . map parseDot
  where
    parseDot textLine =
      let parts = T.splitOn "," textLine
       in Coordinate
            { row = parseUnsignedInt $ parts !! 1
            , column = parseUnsignedInt $ head parts
            }

foldPaper :: TransparentPaper -> Fold -> TransparentPaper
foldPaper paper fold = S.map (apply fold) paper

displayPaper :: TransparentPaper -> String
displayPaper = go $ Coordinate 0 (-1)
  where
    go :: Coordinate -> TransparentPaper -> String
    go cursor toPrint =
      case S.minView toPrint of
        Nothing -> ""
        Just (nextDot, others) ->
          let rowDelta = nextDot.row - cursor.row
              columnDelta =
                nextDot.column
                  - if nextDot.row == cursor.row
                      then cursor.column
                      else 0
           in replicate rowDelta '\n'
                <> replicate (columnDelta - 1) ' '
                <> "#"
                <> go nextDot others
