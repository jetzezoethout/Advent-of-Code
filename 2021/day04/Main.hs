module Main where

import           BingoCard       (losingScore, parseBingoCard, winningScore)
import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseUnsignedInt)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let chunks = splitOn [""] $ T.lines text
        numbers = parseNumbers $ head $ head chunks
        bingoCards = map parseBingoCard $ tail chunks
    print $ winningScore numbers bingoCards
    print $ losingScore numbers bingoCards

parseNumbers :: Text -> [Int]
parseNumbers = map parseUnsignedInt . T.splitOn ","
