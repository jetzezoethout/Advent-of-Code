module MonkeyState where

import           Data.List     (sortBy)
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseUnsignedInt)

data MonkeyState = MonkeyState
  { items   :: Seq Int
  , handled :: Int
  } deriving (Show)

handleItem :: MonkeyState -> MonkeyState
handleItem MonkeyState {..} =
  MonkeyState {items = S.drop 1 items, handled = handled + 1}

receiveItem :: Int -> MonkeyState -> MonkeyState
receiveItem item MonkeyState {..} =
  MonkeyState {items = items |> item, handled = handled}

parseMonkeyState :: Text -> MonkeyState
parseMonkeyState text =
  let itemsPart = T.splitOn ": " text !! 1
      initialItems = map parseUnsignedInt $ T.splitOn ", " itemsPart
   in MonkeyState {items = S.fromList initialItems, handled = 0}

monkeyBusiness :: [MonkeyState] -> Int
monkeyBusiness monkeys =
  let sortedHandled = sortBy (flip compare) $ map handled monkeys
   in product $ take 2 sortedHandled
