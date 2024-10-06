module Eyecolor where

import           Data.Text (Text)

data EyeColor
  = Amber
  | Blue
  | Brown
  | Gray
  | Green
  | Hazel
  | Other

parseEyeColor :: Text -> Maybe EyeColor
parseEyeColor "amb" = Just Amber
parseEyeColor "blu" = Just Blue
parseEyeColor "brn" = Just Brown
parseEyeColor "gry" = Just Gray
parseEyeColor "grn" = Just Green
parseEyeColor "hzl" = Just Hazel
parseEyeColor "oth" = Just Other
parseEyeColor _     = Nothing
