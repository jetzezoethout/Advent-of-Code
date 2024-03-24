module Blueprint where

import           Data.Either    (rights)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read (decimal)

data Blueprint = Blueprint
  { blueprintId      :: Int
  , oreForOre        :: Int
  , oreForClay       :: Int
  , oreForObsidian   :: Int
  , clayForObsidian  :: Int
  , oreForGeode      :: Int
  , obsidianForGeode :: Int
  , maxOre           :: Int
  , maxClay          :: Int
  , maxObsidian      :: Int
  } deriving (Show)

parseBlueprint :: Text -> Blueprint
parseBlueprint text =
  let numbers =
        map fst $ rights $ map (decimal . T.dropAround (== ':')) $ T.words text
   in Blueprint
        { blueprintId = head numbers
        , oreForOre = numbers !! 1
        , oreForClay = numbers !! 2
        , oreForObsidian = numbers !! 3
        , clayForObsidian = numbers !! 4
        , oreForGeode = numbers !! 5
        , obsidianForGeode = numbers !! 6
        , maxOre = maximum [numbers !! 1, numbers !! 2, numbers !! 5]
        , maxClay = numbers !! 4
        , maxObsidian = numbers !! 6
        }
