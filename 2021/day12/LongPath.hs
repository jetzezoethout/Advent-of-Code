module LongPath where

import           Cave          (Cave, CaveSystem, isEndCave, isSmallCave,
                                isStartCave, startCave)
import           Control.Monad (guard)
import           Data.Map      ((!))
import           Data.Maybe    (mapMaybe)
import           Data.Set      (Set)
import qualified Data.Set      as S
import           PathGenerator (PathGenerator (..))

data LongPath = LongPath
  { currentCave        :: Cave
  , forbidden          :: Set Cave
  , hasDoubleSmallCave :: Bool
  }

longPathGenerator :: CaveSystem -> PathGenerator LongPath
longPathGenerator caveSystem =
  PathGenerator
    { start =
        LongPath
          { currentCave = startCave
          , forbidden = S.empty
          , hasDoubleSmallCave = False
          }
    , isEnd = isEndCave . currentCave
    , extend =
        \path -> mapMaybe (extension path) $ caveSystem ! currentCave path
    }
  where
    extension :: LongPath -> Cave -> Maybe LongPath
    extension LongPath {..} target =
      let isAllowed =
            if hasDoubleSmallCave
              then target `S.notMember` forbidden
              else not $ isStartCave target
       in guard isAllowed
            >> Just
                 LongPath
                   { currentCave = target
                   , forbidden =
                       if isSmallCave currentCave
                         then S.insert currentCave forbidden
                         else forbidden
                   , hasDoubleSmallCave =
                       hasDoubleSmallCave || target `S.member` forbidden
                   }
