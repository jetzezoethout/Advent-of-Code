module Main where

import           Constraint      (parseConstraints)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Ticket          (atPosition, illegalNumbers, isValid,
                                  parseTicket)
import           TicketSolver    (solveFields)

main :: IO ()
main =
  processFile $ \text -> do
    let blocks = splitOn [""] $ T.lines text
        constraints = parseConstraints $ head blocks
        myTicket = parseTicket $ (blocks !! 1) !! 1
        nearbyTickets = map parseTicket $ tail $ blocks !! 2
    print $ sum $ nearbyTickets >>= illegalNumbers constraints
    let validTickets = filter (isValid constraints) nearbyTickets
        indexCatalogue = solveFields validTickets constraints
        departureIndices =
          M.elems $ filterOnKey ("departure" `T.isPrefixOf`) indexCatalogue
    print $ product $ map (myTicket `atPosition`) departureIndices

filterOnKey :: Ord k => (k -> Bool) -> Map k v -> Map k v
filterOnKey p = M.filterWithKey (\k _ -> p k)
