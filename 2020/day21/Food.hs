module Food where

import           Data.Set  (Set)
import qualified Data.Set  as S
import           Data.Text (Text)
import qualified Data.Text as T

type Ingredient = Text

type Allergen = Text

data Food = Food
  { ingredients :: Set Ingredient
  , allergens   :: Set Allergen
  } deriving (Show)

parseFood :: Text -> Food
parseFood text =
  let parts = T.splitOn " (contains " text
      ingredients = S.fromList $ T.words $ head parts
      allergens = S.fromList $ T.splitOn ", " $ T.init $ parts !! 1
   in Food {..}
