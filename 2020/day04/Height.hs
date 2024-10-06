module Height where

import           Control.Monad  (guard)
import           Data.Text      (Text)
import           Data.Text.Read

data Unit
  = Cm
  | In

parseUnit :: Text -> Maybe Unit
parseUnit "cm" = Just Cm
parseUnit "in" = Just In
parseUnit _    = Nothing

data Height = Height
  { value :: Int
  , unit  :: Unit
  }

parseHeight :: Text -> Maybe Height
parseHeight text = do
  (value, unitString) <- heightParts
  unit <- parseUnit unitString
  guard $ isValidHeight value unit
  return Height {..}
  where
    heightParts =
      case decimal text of
        Left _       -> Nothing
        Right result -> Just result
    isValidHeight value Cm = 150 <= value && value <= 193
    isValidHeight value In = 59 <= value && value <= 76
