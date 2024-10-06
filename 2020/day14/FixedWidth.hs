module FixedWidth
  ( FixedWidth(contents)
  , fixedWidth
  , leftPad
  , zipFixedWidth
  , branchFixedWidth
  ) where

newtype FixedWidth a = FixedWidth
  { contents :: [a]
  } deriving (Eq, Ord, Show, Functor)

fixedWidth :: [a] -> FixedWidth a
fixedWidth contents =
  if length contents == 36
    then FixedWidth contents
    else error "contents do not have width 36"

leftPad :: a -> [a] -> FixedWidth a
leftPad filler contents =
  let filled = length contents
   in if filled <= 36
        then FixedWidth (replicate (36 - filled) filler <> contents)
        else error "contents exceed width 36"

zipFixedWidth :: (a -> b -> c) -> FixedWidth a -> FixedWidth b -> FixedWidth c
zipFixedWidth f (FixedWidth xs) (FixedWidth ys) = FixedWidth $ zipWith f xs ys

branchFixedWidth :: (a -> [b]) -> FixedWidth a -> [FixedWidth b]
branchFixedWidth f (FixedWidth contents) = map FixedWidth $ go contents
  where
    go []     = [[]]
    go (x:xs) = [leading : rest | leading <- f x, rest <- go xs]
