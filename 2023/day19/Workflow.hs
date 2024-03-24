module Workflow where

import           Data.Map     (Map, fromList)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           WorkflowItem (Target, WorkflowItem (..), parseTarget,
                               parseWorkflowItem)

data Workflow = Workflow
  { steps       :: [WorkflowItem]
  , fallThrough :: Target
  } deriving (Show)

parseWorkflow :: Text -> Workflow
parseWorkflow text =
  Workflow
    { steps = map parseWorkflowItem $ init parts
    , fallThrough = parseTarget $ last parts
    }
  where
    parts = T.split (== ',') text

gatherWorkflows :: [Text] -> Map Text Workflow
gatherWorkflows textLines = fromList $ map parseWorkflowLine textLines
  where
    parseWorkflowLine text =
      let parts = T.split (== '{') text
       in (head parts, parseWorkflow $ T.init $ parts !! 1)
