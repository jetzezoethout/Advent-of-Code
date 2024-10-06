module Main where

import           Data.List.Split (splitOn)
import           Data.Maybe      (mapMaybe)
import qualified Data.Text       as T
import           Passport        (validatePassport)
import           PassportFields  (parsePassportFields)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let passportsWithAllFields =
          mapMaybe parsePassportFields $ splitOn [""] $ T.lines text
        validPassports = mapMaybe validatePassport passportsWithAllFields
    print $ length passportsWithAllFields
    print $ length validPassports
