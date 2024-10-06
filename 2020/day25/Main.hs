module Main where

import           Data.List   (foldl')
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let keys = map parseUnsignedInt $ T.lines text
        publicKeyCard = head keys
        publicKeyDoor = keys !! 1
        encryptionLoopSize =
          (findLoopSize publicKeyCard * findLoopSize publicKeyDoor)
            `mod` (p - 1)
    print $ generateKey encryptionLoopSize
    putStrLn "Pay the deposit!"

p :: Int
p = 20201227

subjectNumber :: Int
subjectNumber = 7

findLoopSize :: Int -> Int
findLoopSize key = go 0 1
  where
    go loops val =
      if val == key
        then loops
        else go (loops + 1) ((val * subjectNumber) `mod` p)

generateKey :: Int -> Int
generateKey loopSize = foldl' (flip ($)) 1 (replicate loopSize loop)
  where
    loop value = (value * subjectNumber) `mod` p
