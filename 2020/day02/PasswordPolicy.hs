module PasswordPolicy where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data PasswordPolicy = PasswordPolicy
  { character       :: Char
  , lowerConstraint :: Int
  , upperConstraint :: Int
  }

parsePasswordPolicy :: Text -> PasswordPolicy
parsePasswordPolicy text =
  let parts = T.words text
      bounds = T.splitOn "-" $ head parts
   in PasswordPolicy
        { character = T.head $ parts !! 1
        , lowerConstraint = parseUnsignedInt $ head bounds
        , upperConstraint = parseUnsignedInt $ bounds !! 1
        }

checkWithBounds :: PasswordPolicy -> Text -> Bool
checkWithBounds PasswordPolicy {..} password =
  let occurences = T.length $ T.filter (== character) password
   in lowerConstraint <= occurences && occurences <= upperConstraint

checkWithPositions :: PasswordPolicy -> Text -> Bool
checkWithPositions PasswordPolicy {..} password =
  (password `T.index` (lowerConstraint - 1) == character)
    `xor` (password `T.index` (upperConstraint - 1) == character)
  where
    xor :: Bool -> Bool -> Bool
    xor = (/=)
