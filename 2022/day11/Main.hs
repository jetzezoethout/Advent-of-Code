module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           InspectionRule  (amendOperation, parseFinalNumber,
                                  parseInspectionRule)

import           Data.Foldable   (foldl')
import           Data.Text       (Text)
import           KeepAwayGame    (monkeyRounds, runGame)
import           MonkeyState     (monkeyBusiness, parseMonkeyState)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let monkeyDescriptions = map (drop 1) $ splitOn [""] $ T.lines text
        initialMonkeyState = map (parseMonkeyState . head) monkeyDescriptions
        basicRules = map (parseInspectionRule . tail) monkeyDescriptions
        monkeyRules = map (amendOperation (`div` 3)) basicRules
        finalMonkeyState =
          runGame (monkeyRounds 20) monkeyRules initialMonkeyState
    print $ monkeyBusiness finalMonkeyState
    let bigDivisor = lcmOfList $ map getDivisor monkeyDescriptions
        extremeMonkeyRules = map (amendOperation (`mod` bigDivisor)) basicRules
        extremeFinalMonkeyState =
          runGame (monkeyRounds 10000) extremeMonkeyRules initialMonkeyState
    print $ monkeyBusiness extremeFinalMonkeyState

getDivisor :: [Text] -> Int
getDivisor = parseFinalNumber . (!! 2)

lcmOfList :: [Int] -> Int
lcmOfList = foldl' lcm 1
