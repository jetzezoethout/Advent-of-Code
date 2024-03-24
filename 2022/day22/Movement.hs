module Movement where

import           Control.Applicative (Alternative (..))
import           Control.Monad.State (StateT (StateT), evalStateT)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read      (decimal)
import           Direction           (Direction, clockWise, counterClockWise)

data Orientation
  = Clockwise
  | Counterclockwise
  deriving (Show)

turn :: Direction -> Orientation -> Direction
direction `turn` Clockwise        = clockWise direction
direction `turn` Counterclockwise = counterClockWise direction

data Movement
  = Turn Orientation
  | Forward Int
  deriving (Show)

type Parser = StateT Text Maybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)      = Nothing
rightToMaybe (Right value) = Just value

intParser :: Parser Int
intParser = StateT $ rightToMaybe . decimal

forwardParser :: Parser Movement
forwardParser = Forward <$> intParser

orientationParser :: Parser Orientation
orientationParser = do
  nextChar <- StateT T.uncons
  case nextChar of
    'R' -> return Clockwise
    'L' -> return Counterclockwise
    _   -> empty

turnParser :: Parser Movement
turnParser = Turn <$> orientationParser

movementParser :: Parser Movement
movementParser = turnParser <|> forwardParser

parseMovements :: Text -> [Movement]
parseMovements = fromJust . evalStateT (many movementParser)
