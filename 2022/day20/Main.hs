module Main where

import           PermutedNumber (decrypt, groveCoordinates, keyd,
                                 parseEncryptedFile)
import           ProcessFile    (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let encryptedFile = parseEncryptedFile text
        decryptedFile = decrypt encryptedFile
        keydEncryptedFile = fmap keyd encryptedFile
        keydDecryptedFile = foldr ($) keydEncryptedFile (replicate 10 decrypt)
    print $ sum $ groveCoordinates decryptedFile
    print $ sum $ groveCoordinates keydDecryptedFile
