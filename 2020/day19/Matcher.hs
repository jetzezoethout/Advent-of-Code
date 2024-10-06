module Matcher where

import           Data.IntMap (IntMap, (!))
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)

data Matcher
  = Match Char
  | All [Matcher]
  | Alternative Matcher Matcher
  deriving (Show)

parseMatcher :: IntMap Text -> Matcher
parseMatcher ruleMap = ruleAt 0
  where
    parse text =
      if T.head text == '"'
        then Match (T.index text 1)
        else case T.splitOn " | " text of
               [x]    -> parseAll x
               [x, y] -> Alternative (parseAll x) (parseAll y)
               _      -> error "illegal matching rule"
    ruleAt index = parse $ ruleMap ! index
    parseAll = All . map (ruleAt . parseUnsignedInt) . T.words
