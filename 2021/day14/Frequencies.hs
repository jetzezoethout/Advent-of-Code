module Frequencies where

import           Data.Map      (Map)
import qualified Data.Map      as M
import           InsertionRule (Pair (..))

type Frequencies = Map Char Int

pairFrequencies :: Pair -> Frequencies
pairFrequencies Pair {..} =
  M.fromList
    $ if left == right
        then [(left, 2)]
        else [(left, 1), (right, 1)]

analysis :: Frequencies -> Int
analysis freqs =
  let freqValues = M.elems freqs
   in maximum freqValues - minimum freqValues
