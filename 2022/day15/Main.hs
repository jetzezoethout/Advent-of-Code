module Main where

import           Coordinate  (Coordinate (..))
import           Data.List   (intersect, nub)
import           Data.Maybe  (mapMaybe)
import qualified Data.Text   as T
import           Diagonal    (borderNEAntidiagonal, borderNWDiagonal,
                              borderSEDiagonal, borderSWAntidiagonal,
                              intersectDiagonals)
import           ProcessFile (processFile)
import           Range       (beaconlessShadow, mergeRanges, size)
import           SensorData  (SensorData (closestBeacon), isPossibleBeacon,
                              parseSensorData)

main :: IO ()
main =
  processFile $ \text -> do
    let sensorDatas = map parseSensorData $ T.lines text
        criticalRow = 2000000
        excludedBySensors =
          sum
            $ map size
            $ mergeRanges
            $ mapMaybe (beaconlessShadow criticalRow) sensorDatas
        beaconsOnCriticalRow =
          length
            $ nub
            $ filter ((== criticalRow) . row)
            $ map closestBeacon sensorDatas
    print $ excludedBySensors - beaconsOnCriticalRow
    let criticalDiagonals =
          nub
            $ map borderNWDiagonal sensorDatas
                `intersect` map borderSEDiagonal sensorDatas
        criticalAntidiagonals =
          nub
            $ map borderSWAntidiagonal sensorDatas
                `intersect` map borderNEAntidiagonal sensorDatas
        possibleBeacons =
          [ intersectDiagonals diagonal antiDiagonal
          | diagonal <- criticalDiagonals
          , antiDiagonal <- criticalAntidiagonals
          ]
    print
      $ tuningFrequency
      $ head
      $ filter (isPossibleBeacon sensorDatas) possibleBeacons

tuningFrequency :: Coordinate -> Int
tuningFrequency Coordinate {..} = 4000000 * column + row
