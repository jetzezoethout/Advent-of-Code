module PolymerTemplate where

import           Cached        (Cached, runMemoized, withCache)
import qualified Data.Map      as M
import           Frequencies   (Frequencies, pairFrequencies)
import           InsertionRule (InsertionRules, Pair (..))

type PolymerTemplate = [Char]

data ExpansionData = ExpansionData
  { initial       :: Pair
  , timesToExpand :: Int
  } deriving (Eq, Ord)

combineWithOverlap :: Char -> Char -> Int -> Int -> Int
combineWithOverlap overlap ch a b =
  if overlap == ch
    then a + b - 1
    else a + b

expandPair ::
     InsertionRules -> ExpansionData -> Cached ExpansionData Frequencies
expandPair rules = withCache go
  where
    go :: ExpansionData -> Cached ExpansionData Frequencies
    go (ExpansionData pair 0) = return $ pairFrequencies pair
    go (ExpansionData pair n) =
      case M.lookup pair rules of
        Nothing -> return $ pairFrequencies pair
        Just middle -> do
          leftExpanded <-
            expandPair rules $ ExpansionData (Pair pair.left middle) (n - 1)
          rightExpanded <-
            expandPair rules $ ExpansionData (Pair middle pair.right) (n - 1)
          return
            $ M.unionWithKey
                (combineWithOverlap middle)
                leftExpanded
                rightExpanded

expandPolymerTemplate :: InsertionRules -> PolymerTemplate -> Int -> Frequencies
expandPolymerTemplate rules text times =
  runMemoized $ go $ zipWith Pair text $ tail text
  where
    go :: [Pair] -> Cached ExpansionData Frequencies
    go [] = return M.empty
    go (first:others) = do
      expandFirstPair <- expandPair rules $ ExpansionData first times
      expandOthers <- go others
      return
        $ M.unionWithKey
            (combineWithOverlap first.right)
            expandFirstPair
            expandOthers
