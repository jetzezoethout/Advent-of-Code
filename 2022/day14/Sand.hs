module Sand where

import           Cave              (Cave, highestRow, sandOrigin)
import           Control.Monad     (guard)
import           Control.Monad.RWS (First (First, getFirst))
import           Coordinate        (Coordinate (..))
import qualified Data.Set          as S
import           Direction         (Direction (..), moveTowards)

type BlockChecker = Coordinate -> Bool

fallOneStep :: BlockChecker -> Coordinate -> Maybe Coordinate
fallOneStep isBlocked coordinate =
  getFirst $ mconcat $ map (First . goTo) possibilities
  where
    oneDown = coordinate `moveTowards` South
    possibilities =
      [oneDown, oneDown `moveTowards` West, oneDown `moveTowards` East]
    goTo :: Coordinate -> Maybe Coordinate
    goTo possibility = [possibility | not $ isBlocked possibility]

fallDown :: Cave -> Coordinate -> Maybe Coordinate
fallDown cave = go
  where
    isBlocked = (`S.member` cave)
    maxRow = highestRow cave
    go current =
      case fallOneStep isBlocked current of
        Nothing   -> Just current
        Just next -> guard (next.row /= maxRow) >> go next

fallUntilSandDisappears :: Cave -> Cave
fallUntilSandDisappears = go
  where
    go :: Cave -> Cave
    go cave =
      case fallDown cave sandOrigin of
        Just restingPlace -> go $ restingPlace `S.insert` cave
        Nothing           -> cave

fallWithFloor :: Cave -> Int -> Coordinate -> Coordinate
fallWithFloor cave caveFloor = go
  where
    isBlocked :: BlockChecker
    isBlocked possibility =
      possibility.row == caveFloor || possibility `S.member` cave
    go current = maybe current go (fallOneStep isBlocked current)

fallUntilIOriginBlocked :: Cave -> Cave
fallUntilIOriginBlocked cave = go cave
  where
    caveFloor = highestRow cave + 2
    go :: Cave -> Cave
    go currentCave =
      if sandOrigin `S.member` currentCave
        then currentCave
        else go
               $ fallWithFloor currentCave caveFloor sandOrigin
                   `S.insert` currentCave
