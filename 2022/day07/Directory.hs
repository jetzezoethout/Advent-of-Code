module Directory where

data Directory = Directory
  { totalSize      :: Int
  , subDirectories :: [Directory]
  } deriving (Show)

insert :: Directory -> Directory -> Directory
insert child parent =
  Directory
    { totalSize = parent.totalSize + child.totalSize
    , subDirectories = child : parent.subDirectories
    }

allSizes :: Directory -> [Int]
allSizes Directory {..} =
  let subSizes = subDirectories >>= allSizes
   in totalSize : subSizes
