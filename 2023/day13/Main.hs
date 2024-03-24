module Main where

import           MirrorData  (score)
import           MirrorEq    (MirrorEquality (..))
import           Pattern     (Pattern, findMirrorsBy, parsePatterns)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let patterns = parsePatterns text
    print $ getAnswer Equal patterns
    print $ getAnswer OffByOne patterns

getAnswer :: MirrorEquality -> [Pattern] -> Int
getAnswer target = sum . map (score . head . findMirrorsBy target)
