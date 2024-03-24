module Graph where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import           Data.List          (foldl')
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T

type Node = Int

type Edge = (Int, Int)

type Graph = IntMap [Node]

parseGraph :: Text -> Graph
parseGraph text =
  makeBirectional $ M.fromList $ map parseGraphLine $ T.lines text
  where
    parseGraphLine textLine =
      let parts = T.splitOn ": " textLine
       in (encode $ head parts, map encode $ T.words $ parts !! 1)

encode :: Text -> Int
encode = encodeThreeChars . T.unpack
  where
    encodeThreeChars [a, b, c] =
      26 * 26 * encodeChar a + 26 * encodeChar b + encodeChar c
    encodeThreeChars _ = error "not three characters"
    encodeChar char = fromEnum char - 96

insertEdge :: Graph -> Edge -> Graph
insertEdge graph (source, target) =
  M.alter (\existing -> Just $ target : fromMaybe [] existing) source graph

makeBirectional :: Graph -> Graph
makeBirectional graph = foldl' enrichNode graph $ M.toList graph
  where
    enrichNode currentGraph (source, targets) =
      foldl' insertEdge currentGraph $ map (, source) targets

deleteFirstOccurrence :: Eq a => a -> [a] -> [a]
deleteFirstOccurrence _ [] = []
deleteFirstOccurrence toRemove (x:xs) =
  if x == toRemove
    then xs
    else x : deleteFirstOccurrence toRemove xs

deleteEdge :: Graph -> Edge -> Graph
deleteEdge graph (source, target) =
  M.adjust (deleteFirstOccurrence target) source graph
