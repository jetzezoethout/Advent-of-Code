module Checker where

import           Control.Monad (guard, (>=>))
import           Data.List     (foldl')
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Matcher       (Matcher (..))

type Checker = Text -> [Text]

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

matchChar :: Char -> Checker
matchChar ch text = do
  (first, rest) <- maybeToList $ T.uncons text
  guard $ first == ch
  return rest

getChecker :: Matcher -> Checker
getChecker (Match ch) = matchChar ch
getChecker (All matchers) = foldl' (>=>) (: []) $ map getChecker matchers
getChecker (Alternative matcher1 matcher2) =
  getChecker matcher1 <> getChecker matcher2

check :: Matcher -> Text -> Bool
check matcher text = "" `elem` getChecker matcher text
