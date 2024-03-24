module Part where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Category
  = X --   #
  | M --  ###
  | A -- #####
  | S --   |
  deriving (Eq, Show)

parseCategory :: Char -> Category
parseCategory 'x' = X
parseCategory 'm' = M
parseCategory 'a' = A
parseCategory 's' = S
parseCategory _   = error "Unknown category"

type Part = Category -> Int

parsePart :: Text -> Part
parsePart text =
  \case
    X -> head numbers
    M -> numbers !! 1
    A -> numbers !! 2
    S -> numbers !! 3
  where
    properContent = T.init $ T.tail text
    numbers = map (parseUnsignedInt . T.drop 2) $ T.split (== ',') properContent

rating :: Part -> Int
rating part = sum $ map part [X, M, A, S]
