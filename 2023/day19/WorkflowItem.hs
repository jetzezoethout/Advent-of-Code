module WorkflowItem where

import           Data.Text (Text)
import qualified Data.Text as T
import           Part      (Category, Part, parseCategory)
import           Range     (Range, firstComplement, liesInside, parseRange)

data FinalState
  = Accepted
  | Rejected
  deriving (Eq, Show)

type Target = Either Text FinalState

parseTarget :: Text -> Target
parseTarget "A"  = Right Accepted
parseTarget "R"  = Right Rejected
parseTarget text = Left text

data Constraint = Constraint
  { category :: Category
  , range    :: Range
  } deriving (Show)

parseConstraint :: Text -> Constraint
parseConstraint text =
  Constraint
    {category = parseCategory $ T.head text, range = parseRange $ T.tail text}

invert :: Constraint -> Constraint
invert Constraint {..} =
  Constraint {category = category, range = firstComplement range}

satisfies :: Part -> Constraint -> Bool
part `satisfies` Constraint {..} = part category `liesInside` range

data WorkflowItem = WorkflowItem
  { constraint :: Constraint
  , goTo       :: Target
  } deriving (Show)

parseWorkflowItem :: Text -> WorkflowItem
parseWorkflowItem text =
  WorkflowItem
    {constraint = parseConstraint $ head parts, goTo = parseTarget $ parts !! 1}
  where
    parts = T.split (== ':') text
