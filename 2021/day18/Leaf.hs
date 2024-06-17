module Leaf where

data Direction
  = L
  | R
  deriving (Show)

factor :: Direction -> Int
factor L = 3
factor R = 2

magnitudeFactor :: [Direction] -> Int
magnitudeFactor = product . map factor

data Leaf = Leaf
  { value :: Int
  , path  :: [Direction]
  } deriving (Show)

magnitudeContribution :: Leaf -> Int
magnitudeContribution Leaf {..} = value * magnitudeFactor path

depth :: Leaf -> Int
depth Leaf {..} = length path

splitLeaf :: Leaf -> [Leaf]
splitLeaf Leaf {..} =
  let halfRoundedDown = value `div` 2
   in [ Leaf {value = halfRoundedDown, path = L : path}
      , Leaf {value = value - halfRoundedDown, path = R : path}
      ]

addToLeaf :: Int -> Leaf -> Leaf
addToLeaf toAdd Leaf {..} = Leaf {value = value + toAdd, path = path}

nest :: Direction -> Leaf -> Leaf
nest dir Leaf {..} = Leaf {value = value, path = path <> [dir]}
