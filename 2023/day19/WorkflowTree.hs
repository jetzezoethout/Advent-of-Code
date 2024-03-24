module WorkflowTree where

import           Data.Map.Strict (Map, (!))
import           Data.Text       (Text)
import           Part            (Category (..), Part)
import           Range           (Range, intersect, maxRange, numberOfElements)
import           Workflow        (Workflow (..), gatherWorkflows)
import           WorkflowItem    (Constraint (..), FinalState (..),
                                  WorkflowItem (..), invert, satisfies)

type PartsCollection = Category -> Range

with :: PartsCollection -> Constraint -> PartsCollection
with partsCollection Constraint {..} cat =
  if cat == category
    then partsCollection cat `intersect` range
    else partsCollection cat

numberOfParts :: PartsCollection -> Int
numberOfParts partsCollection =
  product $ map (numberOfElements . partsCollection) [X, M, A, S]

data WorkflowTree
  = Branch
      { leftConstraint :: Constraint
      , leftBranch     :: WorkflowTree
      , rightBranch    :: WorkflowTree
      }
  | Leaf FinalState
  deriving (Show)

treeify :: Map Text Workflow -> WorkflowTree
treeify workflows = go $ workflows ! "in"
  where
    go Workflow {..} =
      case steps of
        [] ->
          case fallThrough of
            Left label  -> go $ workflows ! label
            Right state -> Leaf state
        (WorkflowItem {..}:remaining) ->
          Branch
            { leftConstraint = constraint
            , leftBranch =
                case goTo of
                  Left label  -> go $ workflows ! label
                  Right state -> Leaf state
            , rightBranch = go $ Workflow remaining fallThrough
            }

parseWorkflowTree :: [Text] -> WorkflowTree
parseWorkflowTree = treeify . gatherWorkflows

processPart :: WorkflowTree -> Part -> FinalState
processPart Branch {..} part =
  if part `satisfies` leftConstraint
    then processPart leftBranch part
    else processPart rightBranch part
processPart (Leaf state) _ = state

numberOfAccepted :: WorkflowTree -> Int
numberOfAccepted tree = go tree $ const maxRange
  where
    go (Leaf state) solutionSpace =
      case state of
        Accepted -> numberOfParts solutionSpace
        Rejected -> 0
    go Branch {..} solutionSpace =
      go leftBranch (solutionSpace `with` leftConstraint)
        + go rightBranch (solutionSpace `with` invert leftConstraint)
