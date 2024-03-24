module CraneType where

type Crate = Char

data CraneType
  = CrateMover9000
  | CrateMover9001

reStack :: CraneType -> [Crate] -> [Crate]
reStack CrateMover9000 = reverse
reStack CrateMover9001 = id
