module RebootStep where

import           Cuboid        (Cuboid, parseCuboid)
import           CuboidCounter (CuboidCounter, emptyCounter, totalCubes,
                                turnOff, turnOn)
import           Data.List     (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T

data Status
  = Off
  | On
  deriving (Eq)

parseStatus :: Text -> Status
parseStatus "off" = Off
parseStatus "on"  = On
parseStatus _     = error "not a valid status"

data RebootStep = RebootStep
  { status :: Status
  , cuboid :: Cuboid
  }

parseRebootStep :: Text -> RebootStep
parseRebootStep text =
  let parts = T.words text
   in RebootStep
        {status = parseStatus $ head parts, cuboid = parseCuboid $ parts !! 1}

applyRebootStep :: CuboidCounter -> RebootStep -> CuboidCounter
applyRebootStep cuboidCounter RebootStep {..} =
  let operation =
        case status of
          On  -> turnOn
          Off -> turnOff
   in operation cuboid cuboidCounter

applyRebootSteps :: [RebootStep] -> Int
applyRebootSteps = totalCubes . foldl' applyRebootStep emptyCounter
