module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           Part            (Part, parsePart, rating)
import           ProcessFile     (processFile)
import           WorkflowItem    (FinalState (Accepted))
import           WorkflowTree    (WorkflowTree, numberOfAccepted,
                                  parseWorkflowTree, processPart)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        chunks = splitOn [""] textLines
        workflowTree = parseWorkflowTree $ head chunks
        parts = map parsePart $ chunks !! 1
    print $ getAnswer1 workflowTree parts
    print $ numberOfAccepted workflowTree

getAnswer1 :: WorkflowTree -> [Part] -> Int
getAnswer1 workflowTree =
  sum . map rating . filter ((== Accepted) . processPart workflowTree)
