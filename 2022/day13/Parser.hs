module Parser where

import           Control.Applicative (Alternative ((<|>)), empty)
import           Control.Monad       (guard)
import           Control.Monad.State (StateT (StateT), evalStateT)
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read      (decimal)
import           Packet              (Packet (IntPacket, ListPacket))

type Parser = StateT Text Maybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)      = Nothing
rightToMaybe (Right value) = Just value

intParser :: Parser Int
intParser = StateT $ rightToMaybe . decimal

readChar :: Parser Char
readChar = StateT T.uncons

intPacketParser :: Parser Packet
intPacketParser = IntPacket <$> intParser

listPacketParser :: Parser Packet
listPacketParser = do
  opener <- readChar
  guard $ opener == '['
  ListPacket <$> (readEmpty <|> readItems)
  where
    readEmpty :: Parser [Packet]
    readEmpty = do
      closer <- readChar
      guard $ closer == ']'
      return []
    readItems :: Parser [Packet]
    readItems = do
      nextItem <- packetParser
      seperator <- readChar
      case seperator of
        ',' -> (nextItem :) <$> readItems
        ']' -> return [nextItem]
        _   -> empty

packetParser :: Parser Packet
packetParser = intPacketParser <|> listPacketParser

parsePacket :: Text -> Packet
parsePacket = fromJust . evalStateT packetParser
