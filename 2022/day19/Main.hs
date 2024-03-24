module Main where

import           Blueprint      (Blueprint (blueprintId), parseBlueprint)
import qualified Data.Text      as T
import           ProcessFile    (processFile)
import           ProductionLine (getMaxGeodes)

main :: IO ()
main =
  processFile $ \text -> do
    let blueprints = map parseBlueprint $ T.lines text
        qualityLevels =
          map
            (\blueprint -> blueprintId blueprint * getMaxGeodes 24 blueprint)
            blueprints
        uneatenBlueprints = take 3 blueprints
    print $ sum qualityLevels
    print $ product $ map (getMaxGeodes 32) uneatenBlueprints
