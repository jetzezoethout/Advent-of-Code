module Main where

import           Data.List       (elemIndex, findIndices, sort)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (fromJust)
import qualified Data.Text       as T
import           Packet          (dividerPacket)
import           Parser          (parsePacket)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let packets = map parsePacket $ filter (/= "") $ T.lines text
    print
      $ sum
      $ map (+ 1)
      $ findIndices (\pair -> head pair < pair !! 1)
      $ chunksOf 2 packets
    let divider1 = dividerPacket 2
        divider2 = dividerPacket 6
        sortedPackets = sort $ divider1 : divider2 : packets
        index1 = fromJust (elemIndex divider1 sortedPackets) + 1
        index2 = fromJust (elemIndex divider2 sortedPackets) + 1
    print $ index1 * index2
