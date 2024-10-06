module CRTSolver where

import           Bus       (Bus (..))
import           Data.List (foldl1')

bezoutCoefficients :: Integral b => b -> b -> (b, b)
bezoutCoefficients a 0 =
  if a == 1
    then (1, 0)
    else error "these numbers do not have gcd 1"
bezoutCoefficients a b =
  let q = a `div` b
      r = a `mod` b
      (x, y) = bezoutCoefficients b r
   in (y, x - q * y)

data ModuloEquation = ModuloEquation
  { remainder :: Integer
  , modulus   :: Integer
  } deriving (Show)

toEquation :: Bus -> ModuloEquation
toEquation Bus {..} =
  ModuloEquation
    { remainder = fromIntegral ((-ordinal) `mod` busId)
    , modulus = fromIntegral busId
    }

solveSystem :: [ModuloEquation] -> ModuloEquation
solveSystem = foldl1' solveTwo
  where
    solveTwo :: ModuloEquation -> ModuloEquation -> ModuloEquation
    solveTwo eq1 eq2 =
      let (x, y) = bezoutCoefficients eq1.modulus eq2.modulus
       in ModuloEquation
            { remainder =
                (x * eq1.modulus * eq2.remainder
                   + y * eq2.modulus * eq1.remainder)
                  `mod` (eq1.modulus * eq2.modulus)
            , modulus = eq1.modulus * eq2.modulus
            }

perfectDepartureTime :: [Bus] -> Integer
perfectDepartureTime = remainder . solveSystem . map toEquation
