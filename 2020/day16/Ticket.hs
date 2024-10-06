module Ticket where

import           Constraint (Constraints, satisfies)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

newtype Ticket = Ticket
  { getNumbers :: [Int]
  } deriving (Show)

parseTicket :: Text -> Ticket
parseTicket = Ticket . map parseUnsignedInt . T.splitOn ","

illegalNumbers :: Constraints -> Ticket -> [Int]
illegalNumbers constraints = filter isIllegal . getNumbers
  where
    isIllegal number = not $ any (number `satisfies`) constraints

isValid :: Constraints -> Ticket -> Bool
isValid constraints = null . illegalNumbers constraints

atPosition :: Ticket -> Int -> Int
ticket `atPosition` idx = getNumbers ticket !! idx
