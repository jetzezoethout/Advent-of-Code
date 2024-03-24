module Navigation where

import           Data.List      (foldl', foldl1')
import           Data.Maybe     (mapMaybe)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read (decimal)
import           Directory      (Directory (..), insert)

data Navigation
  = GoUp
  | GoDown Directory
  deriving (Show)

parseNavigation :: [Text] -> [Navigation]
parseNavigation [] = []
parseNavigation (nextInstruction:otherInstructions) =
  if nextInstruction == "$ cd .."
    then GoUp : parseNavigation otherInstructions
    else let (directoryInfo, remaining) =
               span isTerminalOutput $ tail otherInstructions
          in GoDown (Directory (parseTotalSize directoryInfo) [])
               : parseNavigation remaining
  where
    isTerminalOutput text = T.head text /= '$'

parseTotalSize :: [Text] -> Int
parseTotalSize textLines = sum $ mapMaybe parseFile textLines
  where
    parseFile :: Text -> Maybe Int
    parseFile text =
      case decimal text of
        Left _          -> Nothing
        Right (size, _) -> Just size

type DirectoryPath = [Directory]

navigate :: DirectoryPath -> Navigation -> DirectoryPath
navigate (child:parent:restOfPath) GoUp = insert child parent : restOfPath
navigate _ GoUp                         = error "already at root"
navigate zipper (GoDown nextDirectory)  = nextDirectory : zipper

reconstructFileSystem :: [Navigation] -> Directory
reconstructFileSystem navigations =
  foldl1' insert $ foldl' navigate [] navigations
