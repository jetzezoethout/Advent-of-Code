module Main
  ( main
  ) where

import           Data.List.Split (splitOn)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseUnsignedInt)
import           ProcessFile     (processFile)
import           Range           (Range (..), lowest, parseRanges)
import           RangeMappers    (RangeMappers, parseRangeMappers,
                                  processRangeMappers,
                                  processRangeMappersOnRanges)

main :: IO ()
main =
  processFile $ \text -> do
    let blocks = parseBlocks text
        seeds = parseSeeds $ head $ head blocks
        seedRanges = parseRanges $ head $ head blocks
        mapperDatas = map parseRangeMappers $ tail blocks
    print $ getAnswer1 seeds mapperDatas
    print $ getAnswer2 seedRanges mapperDatas

parseBlocks :: Text -> [[Text]]
parseBlocks = splitOn [""] . T.lines

parseSeeds :: Text -> [Int]
parseSeeds text = map parseUnsignedInt $ T.words $ T.split (== ':') text !! 1

getAnswer1 :: [Int] -> [RangeMappers] -> Int
getAnswer1 seeds mapperdatas =
  minimum $ map (processRangeMappers mapperdatas) seeds

getAnswer2 :: [Range] -> [RangeMappers] -> Int
getAnswer2 seedRanges mapperDatas =
  lowest $ processRangeMappersOnRanges mapperDatas seedRanges
