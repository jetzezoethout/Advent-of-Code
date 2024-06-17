module Interpreter where

import           Chunk     (Chunk, Symbol (..), autocompleteValue,
                            illegalityValue, parseSymbol)
import           Data.List (foldl')
import           Data.Text (Text)
import qualified Data.Text as T

type ChunkStack = [Chunk]

data InterpreterResult
  = Illegal Chunk
  | Incomplete ChunkStack
  deriving (Show)

interpret :: Text -> InterpreterResult
interpret = go [] . map parseSymbol . T.unpack
  where
    go :: ChunkStack -> [Symbol] -> InterpreterResult
    go stack [] = Incomplete stack
    go stack (next:remaining) =
      case next of
        Open chunk -> go (chunk : stack) remaining
        Close chunk ->
          case stack of
            [] -> Illegal chunk
            (top:below) ->
              if chunk == top
                then go below remaining
                else Illegal chunk

illegalityScore :: InterpreterResult -> Maybe Int
illegalityScore (Illegal chunk) = Just $ illegalityValue chunk
illegalityScore (Incomplete _)  = Nothing

autoCompleteScore :: InterpreterResult -> Maybe Int
autoCompleteScore (Illegal _) = Nothing
autoCompleteScore (Incomplete missing) =
  Just $ foldl' (\acc chunk -> 5 * acc + autocompleteValue chunk) 0 missing
