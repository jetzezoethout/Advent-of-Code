module Main where

import           AllergenChart (constructAllergenChart, safeIngredients)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Food          (parseFood)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let foods = map parseFood $ T.lines text
        allergenChart = constructAllergenChart foods
    print $ sum $ map (safeIngredients allergenChart) foods
    TIO.putStrLn $ T.intercalate "," $ M.elems allergenChart
