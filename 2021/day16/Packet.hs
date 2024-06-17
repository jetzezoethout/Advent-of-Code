module Packet where

data Packet
  = LiteralPacket
      { version :: Int
      , value   :: Int
      }
  | OperatorPacket
      { version    :: Int
      , operation  :: [Int] -> Int
      , subPackets :: [Packet]
      }

totalVersion :: Packet -> Int
totalVersion LiteralPacket {..}  = version
totalVersion OperatorPacket {..} = version + sum (map totalVersion subPackets)

evaluate :: Packet -> Int
evaluate LiteralPacket {..}  = value
evaluate OperatorPacket {..} = operation $ map evaluate subPackets

toOperation :: Int -> [Int] -> Int
toOperation 0 xs = sum xs
toOperation 1 xs = product xs
toOperation 2 xs = minimum xs
toOperation 3 xs = maximum xs
toOperation 5 [x, y] =
  if x > y
    then 1
    else 0
toOperation 6 [x, y] =
  if x < y
    then 1
    else 0
toOperation 7 [x, y] =
  if x == y
    then 1
    else 0
toOperation _ _ = error "unknown operation"
