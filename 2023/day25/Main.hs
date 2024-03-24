module Main where

import qualified Data.IntMap.Strict as M
import           Data.Maybe         (fromJust)
import           Data.Monoid        (First (First, getFirst))
import           Graph              (parseGraph)
import           MinCutFinder       (cutGroupWithCost3)
import           ProcessFile        (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let graph = parseGraph text
        nodes = M.keys graph
        group1Size =
          fromJust
            $ getFirst
            $ mconcat
            $ map (First . cutGroupWithCost3 graph (head nodes))
            $ tail nodes
    print $ group1Size * (length nodes - group1Size)
    putStrLn "Push the Big Red Button!"
