module MirrorEq where

data MirrorEquality
  = Equal
  | OffByOne
  | CompletelyDifferent
  deriving (Eq, Show)

instance Semigroup MirrorEquality where
  (<>) :: MirrorEquality -> MirrorEquality -> MirrorEquality
  Equal <> Equal    = Equal
  OffByOne <> Equal = OffByOne
  Equal <> OffByOne = OffByOne
  _ <> _            = CompletelyDifferent

instance Monoid MirrorEquality where
  mempty :: MirrorEquality
  mempty = Equal

class MirrorEq a where
  mirrorEquals :: a -> a -> MirrorEquality

instance MirrorEq a => MirrorEq [a] where
  mirrorEquals :: MirrorEq a => [a] -> [a] -> MirrorEquality
  (x:xs) `mirrorEquals` (y:ys) = x `mirrorEquals` y <> xs `mirrorEquals` ys
  _ `mirrorEquals` _           = Equal
