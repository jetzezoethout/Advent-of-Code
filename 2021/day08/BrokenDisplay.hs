module BrokenDisplay where

import           Data.List     (foldl')
import           Data.Map      (Map, (!))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           DisplaySolver (decrypt)
import           Segment       (Segment, fromChar, toDigit)

data BrokenDisplay = BrokenDisplay
  { decrypter :: Map Segment Segment
  , encrypted :: [[Segment]]
  } deriving (Show)

parseBrokenDisplay :: Text -> BrokenDisplay
parseBrokenDisplay text =
  let parts = T.splitOn " | " text
   in BrokenDisplay
        { decrypter = decrypt $ head parts
        , encrypted = map (map fromChar . T.unpack) $ T.words $ parts !! 1
        }

easyOnes :: BrokenDisplay -> Int
easyOnes = length . filter isEasy . encrypted
  where
    isEasy segments = length segments `elem` [2, 3, 4, 7]

repair :: BrokenDisplay -> Int
repair BrokenDisplay {..} =
  foldl' (\acc digit -> 10 * acc + digit) 0
    $ map (toDigit . map (decrypter !)) encrypted
