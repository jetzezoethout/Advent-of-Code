module Main where

import           Coordinate  (Coordinate (..))
import qualified Data.Text   as T
import           Navigation  (navigate, navigateWithAim, parseNavigation)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let navigations = map parseNavigation $ T.lines text
    print $ score $ navigate navigations
    print $ score $ navigateWithAim navigations

score :: Coordinate -> Int
score Coordinate {..} = row * column
