module DeclarationForm where

import           Data.List (foldl1')
import           Data.Set  (Set, fromList, intersection, size, unions)
import           Data.Text (Text)
import qualified Data.Text as T

newtype DeclarationForm = DeclarationForm
  { answeredQuestionIds :: Set Char
  }

parseDeclarationForm :: Text -> DeclarationForm
parseDeclarationForm = DeclarationForm . fromList . T.unpack

totalAnswers :: [DeclarationForm] -> Int
totalAnswers = size . unions . map answeredQuestionIds

commonAnswers :: [DeclarationForm] -> Int
commonAnswers = size . foldl1' intersection . map answeredQuestionIds
