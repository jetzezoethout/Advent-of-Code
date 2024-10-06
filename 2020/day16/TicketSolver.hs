module TicketSolver where

import           Constraint
import           Data.List  (delete)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Text  (Text)
import           Ticket     (Ticket, atPosition)

possiblePositions :: [Ticket] -> Constraints -> Map Text [Int]
possiblePositions tickets constraints =
  M.map possibilitiesPerConstraint constraints
  where
    numberOfIndices = M.size constraints
    possibilitiesPerConstraint constraint =
      filter (isPossible constraint) [0 .. numberOfIndices - 1]
    isPossible constraint idx =
      all ((`satisfies` constraint) . (`atPosition` idx)) tickets

findUniquePermutation :: (Ord a, Eq b) => Map a [b] -> Map a b
findUniquePermutation = go M.empty
  where
    fromSingleton [x] = Just x
    fromSingleton _   = Nothing
    go :: (Ord a, Eq b) => Map a b -> Map a [b] -> Map a b
    go acc options =
      if M.null options
        then acc
        else case M.lookupMin $ M.mapMaybe fromSingleton options of
               Nothing -> error "no unique permutation"
               Just (field, position) ->
                 go
                   (M.insert field position acc)
                   (M.map (delete position) $ M.delete field options)

solveFields :: [Ticket] -> Constraints -> Map Text Int
solveFields tickets = findUniquePermutation . possiblePositions tickets
