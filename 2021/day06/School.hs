module School where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

newtype School = School
  { getFish :: [Int]
  } deriving (Show)

totalFish :: School -> Int
totalFish = sum . getFish

parseSchool :: Text -> School
parseSchool text = School $ map countFish [0 .. 8]
  where
    fish = map parseUnsignedInt $ T.splitOn "," text
    countFish targetFish = length $ filter (== targetFish) fish

evolve :: School -> School
evolve School {..} =
  School
    $ take 6 (tail getFish)
        <> [head getFish + getFish !! 7, getFish !! 8, head getFish]

evolveTimes :: Int -> School -> School
evolveTimes n = foldr (.) id (replicate n evolve)
