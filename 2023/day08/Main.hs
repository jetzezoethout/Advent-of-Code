module Main where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           DesertMap    (DesertMap, getKeys, parseDesertMap)
import           Direction    (Direction, parseDirections)
import           JourneyState (goOnAdventure)
import           Junction     (Node)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        directionsOnce = parseDirections $ head textLines
        directions = cycle directionsOnce
        desertMap = parseDesertMap $ drop 2 textLines
    print $ getAnswer1 directions desertMap
    print $ getAnswer2 directions desertMap

getAnswer1 :: [Direction] -> DesertMap -> Int
getAnswer1 directions desertMap =
  goOnAdventure directions desertMap (== "ZZZ") "AAA"

initialNodes :: DesertMap -> [Text]
initialNodes = filter ((== 'A') . T.last) . getKeys

isFinalNode :: Node -> Bool
isFinalNode = (== 'Z') . T.last

lcmOfList :: [Int] -> Int
lcmOfList = foldr lcm 1

getAnswer2 :: [Direction] -> DesertMap -> Int
getAnswer2 directions desertMap =
  lcmOfList
    $ map (goOnAdventure directions desertMap isFinalNode)
    $ initialNodes desertMap
