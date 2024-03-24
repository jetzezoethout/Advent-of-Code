module LightDeflector where

import           Direction (Direction (..))

data LightDeflector
  = HorizontalSplitter
  | VerticalSplitter
  | DiagonalMirror
  | AntiDiagonalMirror
  deriving (Show)

fromChar :: Char -> Maybe LightDeflector
fromChar '-'  = Just HorizontalSplitter
fromChar '|'  = Just VerticalSplitter
fromChar '\\' = Just DiagonalMirror
fromChar '/'  = Just AntiDiagonalMirror
fromChar _    = Nothing

exitDirections :: Direction -> Maybe LightDeflector -> [Direction]
exitDirections enteringDirection deflector =
  case deflector of
    Just HorizontalSplitter ->
      case enteringDirection of
        West -> [West]
        East -> [East]
        _    -> [East, West]
    Just VerticalSplitter ->
      case enteringDirection of
        North -> [North]
        South -> [South]
        _     -> [North, South]
    Just DiagonalMirror ->
      [ case enteringDirection of
          North -> West
          West  -> North
          South -> East
          East  -> South
      ]
    Just AntiDiagonalMirror ->
      [ case enteringDirection of
          North -> East
          East  -> North
          South -> West
          West  -> South
      ]
    Nothing -> [enteringDirection]
