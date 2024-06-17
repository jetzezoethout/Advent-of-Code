module Chunk where

data Chunk
  = Round
  | Square
  | Curly
  | Pointy
  deriving (Eq, Show)

illegalityValue :: Chunk -> Int
illegalityValue Round  = 3
illegalityValue Square = 57
illegalityValue Curly  = 1197
illegalityValue Pointy = 25137

autocompleteValue :: Chunk -> Int
autocompleteValue Round  = 1
autocompleteValue Square = 2
autocompleteValue Curly  = 3
autocompleteValue Pointy = 4

data Symbol
  = Open Chunk
  | Close Chunk

parseSymbol :: Char -> Symbol
parseSymbol '(' = Open Round
parseSymbol '[' = Open Square
parseSymbol '{' = Open Curly
parseSymbol '<' = Open Pointy
parseSymbol ')' = Close Round
parseSymbol ']' = Close Square
parseSymbol '}' = Close Curly
parseSymbol '>' = Close Pointy
parseSymbol _   = error "fatal syntax error: I don't know this symbol"
