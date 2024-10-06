module Main where

import qualified Data.Text     as T
import           ProcessFile   (processFile)
import           SavedPassword (isActuallyValid, isValid, parseSavedPassword)

main :: IO ()
main =
  processFile $ \text -> do
    let savedPasswords = map parseSavedPassword $ T.lines text
    print $ length $ filter isValid savedPasswords
    print $ length $ filter isActuallyValid savedPasswords
