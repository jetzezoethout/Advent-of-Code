module MonkeyTree where

import           Data.Map   ((!))
import qualified Data.Map   as M
import           Data.Maybe (isJust)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Operation  (Operation (Minus), execute, parseOperation,
                             solveLeft, solveRight)
import           Parsers    (parseUnsignedInt)

data MonkeyTree
  = Leaf
      { value   :: Int
      , isHuman :: Bool
      }
  | Branch
      { left         :: MonkeyTree
      , right        :: MonkeyTree
      , operation    :: Operation
      , humanTowards :: Maybe Direction
      }
  deriving (Show)

data Direction
  = L
  | R
  deriving (Show)

hasHuman :: MonkeyTree -> Bool
hasHuman Leaf {..}   = isHuman
hasHuman Branch {..} = isJust humanTowards

parseMonkeyTree :: Text -> MonkeyTree
parseMonkeyTree text = go "root"
  where
    monkeyMap =
      M.fromList
        $ map ((\xs -> (head xs, xs !! 1)) . T.splitOn ": ")
        $ T.lines text
    go monkeyName =
      case T.words $ monkeyMap ! monkeyName of
        [number] ->
          Leaf {value = parseUnsignedInt number, isHuman = monkeyName == "humn"}
        [leftMonkey, opName, rightMonkey] ->
          let left = go leftMonkey
              right = go rightMonkey
           in Branch
                { left = left
                , right = right
                , operation = parseOperation opName
                , humanTowards =
                    if hasHuman left
                      then Just L
                      else (if hasHuman right
                              then Just R
                              else Nothing)
                }
        _ -> error "instructions unclear, monkey on fire"

evaluate :: MonkeyTree -> Int
evaluate Leaf {..} = value
evaluate Branch {..} =
  let op = execute operation
   in evaluate left `op` evaluate right

adjustRoot :: MonkeyTree -> MonkeyTree
adjustRoot Branch {..} =
  Branch
    {operation = Minus, left = left, right = right, humanTowards = humanTowards}
adjustRoot leaf = leaf

findHumanValue :: MonkeyTree -> Int
findHumanValue = go 0
  where
    go target Leaf {..} =
      if isHuman
        then target
        else error "monkey, not a human"
    go target Branch {..} =
      case humanTowards of
        Nothing -> error "only monkeys here"
        Just L ->
          let op = solveLeft operation
           in go (target `op` evaluate right) left
        Just R ->
          let op = solveRight operation
           in go (target `op` evaluate left) right
