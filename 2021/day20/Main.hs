module Main where

import           Algorithm   (parseAlgorithm)
import qualified Data.Text   as T
import           Image       (enhance, lightPixels, parseImage)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        algorithm = parseAlgorithm $ head textLines
        initialImage = parseImage $ drop 2 textLines
        enhancedImage = applyTimes 2 (enhance algorithm) initialImage
        superEnhancedImage = applyTimes 50 (enhance algorithm) initialImage
    print $ lightPixels enhancedImage
    print $ lightPixels superEnhancedImage

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f x = foldr ($) x $ replicate n f
