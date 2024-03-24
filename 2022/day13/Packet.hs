module Packet where

data Packet
  = IntPacket Int
  | ListPacket [Packet]
  deriving (Eq, Show)

dividerPacket :: Int -> Packet
dividerPacket n = ListPacket [toListPacket n]

toListPacket :: Int -> Packet
toListPacket n = ListPacket [IntPacket n]

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  IntPacket left `compare` IntPacket right = left `compare` right
  IntPacket left `compare` right = toListPacket left `compare` right
  left `compare` IntPacket right = left `compare` toListPacket right
  ListPacket left `compare` ListPacket right = left `compLists` right
    where
      compLists :: [Packet] -> [Packet] -> Ordering
      [] `compLists` []         = EQ
      [] `compLists` _          = LT
      _ `compLists` []          = GT
      (x:xs) `compLists` (y:ys) = x `compare` y <> xs `compLists` ys
