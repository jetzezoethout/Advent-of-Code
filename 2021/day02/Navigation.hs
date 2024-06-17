module Navigation where

import           Coordinate    (Coordinate (..))
import           Data.Foldable (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Direction     (Direction (..), moveTowardsBy)
import           Parsers       (parseUnsignedInt)

data Navigation
  = Forward Int
  | Down Int
  | Up Int

parseNavigation :: Text -> Navigation
parseNavigation text =
  let parts = T.words text
      amount = parseUnsignedInt $ parts !! 1
   in case head parts of
        "forward" -> Forward amount
        "down"    -> Down amount
        "up"      -> Up amount
        _         -> error "instructions unclear, submarine on fire"

applyNavigation :: Coordinate -> Navigation -> Coordinate
applyNavigation position (Forward amount) = moveTowardsBy position East amount
applyNavigation position (Down amount)    = moveTowardsBy position South amount
applyNavigation position (Up amount)      = moveTowardsBy position North amount

navigate :: [Navigation] -> Coordinate
navigate = foldl' applyNavigation $ Coordinate 0 0

navigateWithAim :: [Navigation] -> Coordinate
navigateWithAim = go (Coordinate 0 0) 0
  where
    go position _ [] = position
    go position aim (navigation:remaining) =
      case navigation of
        Forward amount ->
          go
            (moveTowardsBy
               (moveTowardsBy position South $ aim * amount)
               East
               amount)
            aim
            remaining
        Down amount -> go position (aim + amount) remaining
        Up amount -> go position (aim - amount) remaining
