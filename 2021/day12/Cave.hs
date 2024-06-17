module Cave where

import           Data.Char (isLower)
import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T

newtype Cave = Cave
  { getLabel :: Text
  } deriving (Eq, Ord, Show)

startCave :: Cave
startCave = Cave "start"

isStartCave :: Cave -> Bool
isStartCave Cave {..} = getLabel == "start"

isEndCave :: Cave -> Bool
isEndCave Cave {..} = getLabel == "end"

isSmallCave :: Cave -> Bool
isSmallCave Cave {..} = isLower $ T.head getLabel

type CaveSystem = Map Cave [Cave]

parseCaveSystem :: Text -> CaveSystem
parseCaveSystem = foldr processTunnel M.empty . T.lines
  where
    processTunnel textLine =
      let parts = T.splitOn "-" textLine
          cave1 = Cave $ head parts
          cave2 = Cave $ parts !! 1
       in M.alter (addCave cave2) cave1 . M.alter (addCave cave1) cave2
    addCave newCave Nothing         = Just [newCave]
    addCave newCave (Just existing) = Just $ newCave : existing
