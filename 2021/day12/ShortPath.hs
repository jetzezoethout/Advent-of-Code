module ShortPath where

import           Cave          (Cave, CaveSystem, isEndCave, isSmallCave,
                                startCave)
import           Data.Map      ((!))
import           Data.Maybe    (mapMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           PathGenerator (PathGenerator (..))

data ShortPath = ShortPath
  { currentCave :: Cave
  , forbidden   :: Set Cave
  }

shortPathGenerator :: CaveSystem -> PathGenerator ShortPath
shortPathGenerator caveSystem =
  PathGenerator
    { start = ShortPath {currentCave = startCave, forbidden = S.empty}
    , isEnd = isEndCave . currentCave
    , extend =
        \path -> mapMaybe (extension path) $ caveSystem ! currentCave path
    }
  where
    extension :: ShortPath -> Cave -> Maybe ShortPath
    extension ShortPath {..} target =
      [ ShortPath
        { currentCave = target
        , forbidden =
            if isSmallCave currentCave
              then S.insert currentCave forbidden
              else forbidden
        }
      | target `S.notMember` forbidden
      ]
