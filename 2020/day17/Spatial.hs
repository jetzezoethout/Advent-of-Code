module Spatial where

class Ord a =>
      Spatial a
  where
  neighbours :: a -> [a]
