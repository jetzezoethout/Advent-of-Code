module Algorithm where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, fromList, (!))
import           Pixel       (Pixel, fromChar, toIndex)

newtype Algorithm = Algorithm
  { pixelMap :: Vector Pixel
  } deriving (Show)

decode :: Algorithm -> [Pixel] -> Pixel
decode Algorithm {..} pixels = pixelMap ! toIndex pixels

parseAlgorithm :: Text -> Algorithm
parseAlgorithm = Algorithm . fromList . map fromChar . T.unpack

darkVoid :: Algorithm -> Pixel
darkVoid Algorithm {..} = pixelMap ! 0

lightVoid :: Algorithm -> Pixel
lightVoid Algorithm {..} = pixelMap ! 511
