module InsertionRule where

import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T

data Pair = Pair
  { left  :: Char
  , right :: Char
  } deriving (Eq, Ord, Show)

type InsertionRules = Map Pair Char

parseInsertionRules :: [Text] -> InsertionRules
parseInsertionRules = M.fromList . map parseRule
  where
    parseRule textLine =
      ( Pair {left = T.head textLine, right = textLine `T.index` 1}
      , T.last textLine)
