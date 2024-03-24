module Main where

import           Board       (initialState, moveWithWrap, parseBoard)
import qualified Data.Text   as T
import           Faces       (moveWithCube)
import           Movement    (parseMovements)
import           ProcessFile (processFile)
import           WalkState   (password, walk)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        board = parseBoard $ take (length textLines - 2) textLines
        movements = parseMovements $ last textLines
    print $ password $ walk (moveWithWrap board) (initialState board) movements
    print $ password $ walk (moveWithCube board) (initialState board) movements
