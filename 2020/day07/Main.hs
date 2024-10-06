module Main where

import           BagageRules (containsShinyGoldBag, parseBagageRules,
                              totalContents)
import qualified Data.Map    as M
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let bagageRules = parseBagageRules text
    print
      $ length
      $ filter (containsShinyGoldBag bagageRules)
      $ M.keys bagageRules
    print $ totalContents bagageRules "shiny gold"
