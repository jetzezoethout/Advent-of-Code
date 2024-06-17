module Main where

import           Bit         (parseBits)
import           BitsParser  (parsePacket)
import           Packet      (evaluate, totalVersion)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let packet = parsePacket $ parseBits text
    print $ totalVersion packet
    print $ evaluate packet
