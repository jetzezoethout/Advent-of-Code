module Passport where

import           Control.Monad  (guard)
import           Data.Char      (isNumber)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read (decimal)
import           Eyecolor       (EyeColor, parseEyeColor)
import           Height         (Height, parseHeight)
import           PassportFields (PassportFields (..))

data Passport = Passport
  { birthYear      :: Int
  , issueYear      :: Int
  , expirationYear :: Int
  , height         :: Height
  , hairColor      :: Text
  , eyeColor       :: EyeColor
  , passportId     :: Text
  }

validatePassport :: PassportFields -> Maybe Passport
validatePassport PassportFields {..} = do
  birthYear <- parseBoundedInt 1920 2002 birthYearField
  issueYear <- parseBoundedInt 2010 2020 issueYearField
  expirationYear <- parseBoundedInt 2020 2030 expirationYearField
  height <- parseHeight heightField
  hairColor <- validateHairColor hairColorField
  eyeColor <- parseEyeColor eyeColorField
  passportId <- validatePassportId passportIdField
  return Passport {..}

parseBoundedInt :: Int -> Int -> Text -> Maybe Int
parseBoundedInt lower upper text =
  case decimal text of
    Left _            -> Nothing
    Right (result, _) -> [result | lower <= result && result <= upper]

validateHairColor :: Text -> Maybe Text
validateHairColor text = do
  (leading, rest) <- T.uncons text
  guard $ leading == '#'
  guard $ T.length rest == 6
  guard $ T.all isAllowed rest
  return text
  where
    isAllowed ch = ch `elem` ['0' .. '9'] <> ['a' .. 'f']

validatePassportId :: Text -> Maybe Text
validatePassportId text = do
  guard $ T.length text == 9
  guard $ T.all isNumber text
  return text
