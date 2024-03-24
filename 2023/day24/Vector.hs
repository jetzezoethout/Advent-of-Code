module Vector where

import           Control.Monad (guard)
import           Data.Ratio    ((%))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseIntegral)

data Two

data Three

newtype Vector size a = Vector
  { coefficients :: [a]
  } deriving (Show, Functor, Foldable)

parseVector :: Text -> Vector size Integer
parseVector = Vector . map (parseIntegral . T.strip) . T.split (== ',')

x :: Vector size a -> a
x = head . coefficients

y :: Vector size a -> a
y = (!! 1) . coefficients

z :: Vector Three a -> a
z = (!! 2) . coefficients

add :: Num a => Vector size a -> Vector size a -> Vector size a
add (Vector xs) (Vector ys) = Vector $ zipWith (+) xs ys

minus :: Num a => Vector size a -> Vector size a
minus = fmap negate

scalar :: Num a => a -> Vector size a -> Vector size a
scalar l = fmap (l *)

toFrac :: Vector size Integer -> Vector size Rational
toFrac = ((% 1) <$>)

checkBounds :: Ord a => a -> a -> Vector size a -> Bool
checkBounds lower upper (Vector xs) =
  all (\component -> lower <= component && component <= upper) xs

det2 :: Num a => Vector Two a -> Vector Two a -> a
det2 v1 v2 = x v1 * y v2 - x v2 * y v1

dropX :: Vector Three a -> Vector Two a
dropX (Vector xs) = Vector $ tail xs

dropY :: Vector Three a -> Vector Two a
dropY (Vector xs) = Vector $ head xs : drop 2 xs

dropZ :: Vector Three a -> Vector Two a
dropZ (Vector xs) = Vector $ take 2 xs

innerProduct :: Num a => Vector size a -> Vector size a -> a
innerProduct (Vector xs) (Vector ys) = sum $ zipWith (*) xs ys

crossProduct :: Num a => Vector Three a -> Vector Three a -> Vector Three a
crossProduct v1 v2 =
  Vector
    [ det2 (dropX v1) (dropX v2)
    , det2 (dropY v2) (dropY v1)
    , det2 (dropZ v1) (dropZ v2)
    ]

solve2 :: Vector Three (Vector Two Integer) -> Maybe (Vector Two Rational)
solve2 columns =
  let coefficients1 = x columns
      coefficients2 = y columns
      constants = z columns
      determinant = det2 coefficients1 coefficients2
   in guard (determinant /= 0)
        >> Just
             ((% determinant)
                <$> Vector
                      [ det2 constants coefficients2
                      , det2 coefficients1 constants
                      ])
