module PassportFields where

import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T

data PassportFields = PassportFields
  { birthYearField      :: Text
  , issueYearField      :: Text
  , expirationYearField :: Text
  , heightField         :: Text
  , hairColorField      :: Text
  , eyeColorField       :: Text
  , passportIdField     :: Text
  }

parsePassportFields :: [Text] -> Maybe PassportFields
parsePassportFields textLines = do
  birthYearField <- M.lookup "byr" fields
  issueYearField <- M.lookup "iyr" fields
  expirationYearField <- M.lookup "eyr" fields
  heightField <- M.lookup "hgt" fields
  hairColorField <- M.lookup "hcl" fields
  eyeColorField <- M.lookup "ecl" fields
  passportIdField <- M.lookup "pid" fields
  return PassportFields {..}
  where
    fields = M.fromList (textLines >>= (map keyValuePair . T.words))
    keyValuePair :: Text -> (Text, Text)
    keyValuePair text =
      let parts = T.splitOn ":" text
       in (head parts, parts !! 1)
