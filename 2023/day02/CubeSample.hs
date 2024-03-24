module CubeSample where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data CubeSample = CubeSample
  { redCubes   :: Int
  , blueCubes  :: Int
  , greenCubes :: Int
  } deriving (Show)

isBoundedBy :: CubeSample -> CubeSample -> Bool
CubeSample {..} `isBoundedBy` (CubeSample redBound blueBound greenBound) =
  redCubes <= redBound && blueCubes <= blueBound && greenCubes <= greenBound

emptySample :: CubeSample
emptySample = CubeSample 0 0 0

parseSample :: Text -> CubeSample
parseSample = processCubeAmounts . map T.strip . T.split (== ',')

parseSamples :: Text -> [CubeSample]
parseSamples = map (parseSample . T.strip) . T.split (== ';')

processCubeAmount :: Text -> CubeSample -> CubeSample
processCubeAmount text initialSample@CubeSample {..} =
  let parts = T.split (== ' ') text
      numberOfCubes = parseUnsignedInt $ head parts
      kind = parts !! 1
   in case kind of
        "red"   -> initialSample {redCubes = redCubes + numberOfCubes}
        "blue"  -> initialSample {blueCubes = blueCubes + numberOfCubes}
        "green" -> initialSample {greenCubes = greenCubes + numberOfCubes}
        _       -> initialSample

processCubeAmounts :: [Text] -> CubeSample
processCubeAmounts = foldr processCubeAmount emptySample

union :: CubeSample -> CubeSample -> CubeSample
union sample1 sample2 =
  CubeSample
    { redCubes = max (redCubes sample1) (redCubes sample2)
    , blueCubes = max (blueCubes sample1) (blueCubes sample2)
    , greenCubes = max (greenCubes sample1) (greenCubes sample2)
    }

leastUpperBound :: [CubeSample] -> CubeSample
leastUpperBound = foldr union emptySample

power :: CubeSample -> Int
power CubeSample {..} = redCubes * blueCubes * greenCubes
