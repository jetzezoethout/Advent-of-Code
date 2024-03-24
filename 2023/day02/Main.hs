module Main where

import           CubeSample  (CubeSample (..), leastUpperBound, power)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Game        (Game (gameId, samples), isPossibleGiven,
                              parseGame)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let games = getGames text
    print $ getAnswer1 games
    print $ getAnswer2 games

givenBound :: CubeSample
givenBound = CubeSample {redCubes = 12, blueCubes = 14, greenCubes = 13}

getGames :: Text -> [Game]
getGames = map parseGame . T.lines

getAnswer1 :: [Game] -> Int
getAnswer1 = sum . map gameId . filter (`isPossibleGiven` givenBound)

getAnswer2 :: [Game] -> Int
getAnswer2 = sum . map (power . leastUpperBound . samples)
