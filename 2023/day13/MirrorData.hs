module MirrorData where

data Orientation
  = Horizontal
  | Vertical
  deriving (Show)

flipOrientation :: Orientation -> Orientation
flipOrientation Horizontal = Vertical
flipOrientation Vertical   = Horizontal

data MirrorData = MirrorData
  { orientation    :: Orientation
  , mirrorPosition :: Int
  } deriving (Show)

transposeMirror :: MirrorData -> MirrorData
transposeMirror MirrorData {..} =
  MirrorData
    {orientation = flipOrientation orientation, mirrorPosition = mirrorPosition}

score :: MirrorData -> Int
score MirrorData {..} =
  mirrorPosition
    * case orientation of
        Vertical   -> 1
        Horizontal -> 100
