module Main where

import           Checker         (check)
import           Data.IntMap     (IntMap, fromList)
import qualified Data.IntMap     as M
import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Matcher         (parseMatcher)
import           Parsers         (parseUnsignedInt)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        rules = gatherByIndex $ head parts
        matcher = parseMatcher rules
        messages = parts !! 1
    print $ length $ filter (check matcher) messages
    let correctedRules =
          M.insert 8 "42 | 42 8" $ M.insert 11 "42 31 | 42 11 31" rules
        correctedMatcher = parseMatcher correctedRules
    print $ length $ filter (check correctedMatcher) messages

gatherByIndex :: [Text] -> IntMap Text
gatherByIndex textLines = fromList $ map parseLine textLines
  where
    parseLine textLine =
      let parts = T.splitOn ": " textLine
       in (parseUnsignedInt $ head parts, parts !! 1)
