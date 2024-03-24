module ProductionLine where

import           Blueprint
import           Control.Monad
import           Data.Maybe

data ProductionLine = ProductionLine
  { minutesLeft     :: Int
  , currentOre      :: Int
  , currentClay     :: Int
  , currentObsidian :: Int
  , oreRobots       :: Int
  , clayRobots      :: Int
  , obsidianRobots  :: Int
  , totalGeodes     :: Int
  } deriving (Show)

initialSituation :: Int -> ProductionLine
initialSituation initialMinutes =
  ProductionLine
    { minutesLeft = initialMinutes
    , currentOre = 0
    , currentClay = 0
    , currentObsidian = 0
    , oreRobots = 1
    , clayRobots = 0
    , obsidianRobots = 0
    , totalGeodes = 0
    }

ceilDiv :: Integral a => a -> a -> a
a `ceilDiv` b = ((a - 1) `div` b) + 1

buyOreRobot :: Blueprint -> ProductionLine -> Maybe ProductionLine
buyOreRobot Blueprint {..} line@ProductionLine {..} = do
  guard $ oreRobots < maxOre
  let minutesNeeded = 1 + max 0 ((oreForOre - currentOre) `ceilDiv` oreRobots)
      newMinutesLeft = minutesLeft - minutesNeeded
  guard $ newMinutesLeft >= 3
  return
    $ line
        { minutesLeft = newMinutesLeft
        , currentOre = currentOre + oreRobots * minutesNeeded - oreForOre
        , currentClay = currentClay + clayRobots * minutesNeeded
        , currentObsidian = currentObsidian + obsidianRobots * minutesNeeded
        , oreRobots = oreRobots + 1
        }

buyClayRobot :: Blueprint -> ProductionLine -> Maybe ProductionLine
buyClayRobot Blueprint {..} line@ProductionLine {..} = do
  guard $ clayRobots < maxClay
  let minutesNeeded = 1 + max 0 ((oreForClay - currentOre) `ceilDiv` oreRobots)
      newMinutesLeft = minutesLeft - minutesNeeded
  guard $ newMinutesLeft >= 5
  return
    $ line
        { minutesLeft = newMinutesLeft
        , currentOre = currentOre + oreRobots * minutesNeeded - oreForClay
        , currentClay = currentClay + clayRobots * minutesNeeded
        , currentObsidian = currentObsidian + obsidianRobots * minutesNeeded
        , clayRobots = clayRobots + 1
        }

buyObsidianRobot :: Blueprint -> ProductionLine -> Maybe ProductionLine
buyObsidianRobot Blueprint {..} line@ProductionLine {..} = do
  guard $ clayRobots > 0
  guard $ obsidianRobots < maxObsidian
  let oreMinutesNeeded = (oreForObsidian - currentOre) `ceilDiv` oreRobots
      clayMinutesNeeded = (clayForObsidian - currentClay) `ceilDiv` clayRobots
      minutesNeeded = 1 + max 0 (max oreMinutesNeeded clayMinutesNeeded)
      newMinutesLeft = minutesLeft - minutesNeeded
  guard $ newMinutesLeft >= 3
  return
    $ line
        { minutesLeft = newMinutesLeft
        , currentOre = currentOre + oreRobots * minutesNeeded - oreForObsidian
        , currentClay =
            currentClay + clayRobots * minutesNeeded - clayForObsidian
        , currentObsidian = currentObsidian + obsidianRobots * minutesNeeded
        , obsidianRobots = obsidianRobots + 1
        }

buyGeodeRobot :: Blueprint -> ProductionLine -> Maybe ProductionLine
buyGeodeRobot Blueprint {..} line@ProductionLine {..} = do
  guard $ obsidianRobots > 0
  guard $ obsidianRobots < maxObsidian
  let oreMinutesNeeded = (oreForGeode - currentOre) `ceilDiv` oreRobots
      obsidianMinutesNeeded =
        (obsidianForGeode - currentObsidian) `ceilDiv` obsidianRobots
      minutesNeeded = 1 + max 0 (max oreMinutesNeeded obsidianMinutesNeeded)
      newMinutesLeft = minutesLeft - minutesNeeded
  guard $ newMinutesLeft >= 1
  return
    $ line
        { minutesLeft = newMinutesLeft
        , currentOre = currentOre + oreRobots * minutesNeeded - oreForGeode
        , currentClay = currentClay + clayRobots * minutesNeeded
        , currentObsidian =
            currentObsidian + obsidianRobots * minutesNeeded - obsidianForGeode
        , totalGeodes = totalGeodes + newMinutesLeft
        }

getMaxGeodes :: Int -> Blueprint -> Int
getMaxGeodes initialMinutes blueprint = go $ initialSituation initialMinutes
  where
    go :: ProductionLine -> Int
    go currentSituation =
      case mapMaybe
             (\buyFunction -> buyFunction blueprint currentSituation)
             [buyGeodeRobot, buyObsidianRobot, buyClayRobot, buyOreRobot] of
        []            -> totalGeodes currentSituation
        possibilities -> maximum $ map go possibilities
