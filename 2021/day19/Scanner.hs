module Scanner where

import           Control.Monad (guard)
import           Data.Function (on)
import           Data.List     (find, intersectBy)
import           Data.Maybe    (fromJust)
import           Data.Text     (Text)
import           Point         (CachedNormalVector (..), Point,
                                allNormalVectors, along, origin, parsePoint, to)
import           Vector        (apply, findRotation)

data RelativeScanner = RelativeScanner
  { relBeacons       :: [Point]
  , relNormalVectors :: [CachedNormalVector]
  }

data AbsoluteScanner = AbsoluteScanner
  { absBeacons         :: [Point]
  , absNormalVectors   :: [CachedNormalVector]
  , absScannerPosition :: Point
  }

parseScanner :: [Text] -> RelativeScanner
parseScanner textLines =
  RelativeScanner
    {relBeacons = beacons, relNormalVectors = allNormalVectors beacons}
  where
    beacons = map parsePoint $ tail textLines

pin :: RelativeScanner -> AbsoluteScanner
pin RelativeScanner {..} =
  AbsoluteScanner
    { absBeacons = relBeacons
    , absNormalVectors = relNormalVectors
    , absScannerPosition = origin
    }

align :: AbsoluteScanner -> RelativeScanner -> Maybe AbsoluteScanner
align AbsoluteScanner {..} RelativeScanner {..} =
  guard (length commonNormals >= 66)
    >> Just
         AbsoluteScanner
           { absBeacons = beacons
           , absNormalVectors = allNormalVectors beacons
           , absScannerPosition = scannerPosition
           }
  where
    commonNormals =
      intersectBy ((==) `on` normalVector) absNormalVectors relNormalVectors
    CachedNormalVector absPoint1 absPoint2 commonNormal = head commonNormals
    CachedNormalVector relPoint1 relPoint2 _ =
      fromJust $ find ((== commonNormal) . normalVector) relNormalVectors
    absVector = absPoint1 `to` absPoint2
    (correctRelPoint, rotation) =
      case findRotation (relPoint1 `to` relPoint2) absVector of
        Just r -> (relPoint1, r)
        Nothing ->
          ( relPoint2
          , fromJust $ findRotation (relPoint2 `to` relPoint1) absVector)
    scannerPosition =
      absPoint1 `along` apply rotation (correctRelPoint `to` origin)
    beacons =
      map
        ((scannerPosition `along`) . apply rotation . (origin `to`))
        relBeacons

reconstructMap :: [RelativeScanner] -> [AbsoluteScanner]
reconstructMap [] = []
reconstructMap (first:others) = pin first : go [pin first] others
  where
    go :: [AbsoluteScanner] -> [RelativeScanner] -> [AbsoluteScanner]
    go _ [] = []
    go queue relativeScanners =
      case queue of
        [] -> error "not enough overlap to reconstruct the map"
        current:remainingQueue ->
          let (newAbsolute, stillRelative) = explore current relativeScanners
           in newAbsolute <> go (newAbsolute <> remainingQueue) stillRelative
    explore ::
         AbsoluteScanner
      -> [RelativeScanner]
      -> ([AbsoluteScanner], [RelativeScanner])
    explore _ [] = ([], [])
    explore base (relative:remaining) =
      let (absolutes, relatives) = explore base remaining
       in case align base relative of
            Nothing       -> (absolutes, relative : relatives)
            Just absolute -> (absolute : absolutes, relatives)
