module Snafu where

import           Data.List  (elemIndex, foldl')
import           Data.Maybe (fromJust)

snafuDigits :: String
snafuDigits = "=-012"

fromDigit :: Char -> Int
fromDigit ch = fromJust (elemIndex ch snafuDigits) - 2

fromSnafu :: String -> Int
fromSnafu = foldl' (\acc nextDigit -> 5 * acc + fromDigit nextDigit) 0

toDigit :: Int -> Char
toDigit n = snafuDigits !! (n + 2)

toSnafu :: Int -> String
toSnafu = go ""
  where
    go acc 0 = acc
    go acc n =
      let units = ((n + 2) `mod` 5) - 2
       in go (toDigit units : acc) $ (n - units) `div` 5
