module Junction where

import           Data.Text (Text)
import           Direction (Direction (..))

type Node = Text

data Junction = Junction
  { leftNode  :: Node
  , rightNode :: Node
  } deriving (Show)

takeJunction :: Direction -> Junction -> Node
takeJunction direction Junction {..} =
  case direction of
    L -> leftNode
    R -> rightNode
