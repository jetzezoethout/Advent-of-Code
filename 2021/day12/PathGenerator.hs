module PathGenerator where

data PathGenerator path = PathGenerator
  { start  :: path
  , isEnd  :: path -> Bool
  , extend :: path -> [path]
  }

allPaths :: PathGenerator path -> Int
allPaths PathGenerator {..} = go 0 [start]
  where
    go found [] = found
    go found (next:others) =
      if isEnd next
        then go (found + 1) others
        else go found $ extend next <> others
