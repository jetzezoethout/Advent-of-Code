module DesertMap where

import           Data.Char       (isPunctuation, isSymbol)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Junction        (Junction (Junction), Node)

newtype DesertMap = DesertMap
  { getMap :: Map Node Junction
  }

parseNodeAndJunction :: Text -> (Node, Junction)
parseNodeAndJunction text =
  let shouldPreserve char = not (isPunctuation char) && not (isSymbol char)
      strippedText = T.filter shouldPreserve text
      parts = T.words strippedText
   in (head parts, Junction (parts !! 1) (parts !! 2))

parseDesertMap :: [Text] -> DesertMap
parseDesertMap = DesertMap . M.fromList . map parseNodeAndJunction

findJunction :: DesertMap -> Node -> Junction
findJunction desertMap node =
  fromMaybe (error "junction not found") $ M.lookup node $ getMap desertMap

getKeys :: DesertMap -> [Node]
getKeys = map fst . M.toList . getMap
