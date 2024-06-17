module PhaseSpace where

import           TargetArea (TargetArea, hasMissed, isInside, maxXVel, maxYVel,
                             minXVel, minYVel)

data PhaseSpace = PhaseSpace
  { xPos :: Int
  , yPos :: Int
  , xVel :: Int
  , yVel :: Int
  }

initialState :: Int -> Int -> PhaseSpace
initialState = PhaseSpace 0 0

evolve :: PhaseSpace -> PhaseSpace
evolve PhaseSpace {..} =
  PhaseSpace
    {xPos = xPos + xVel, yPos = yPos + yVel, xVel = drag xVel, yVel = yVel - 1}

drag :: Int -> Int
drag 0        = 0
drag positive = positive - 1

hitsTarget :: TargetArea -> (Int, Int) -> Bool
hitsTarget targetArea (initialXVel, initialYVel) =
  go (initialState initialXVel initialYVel)
  where
    go state@PhaseSpace {..}
      | isInside targetArea xPos yPos = True
      | hasMissed targetArea xPos yPos = False
      | otherwise = go $ evolve state

initialVelocities :: TargetArea -> Int
initialVelocities targetArea =
  length
    $ filter
        (hitsTarget targetArea)
        [ (xVel, yVel)
        | xVel <- [minXVel targetArea .. maxXVel targetArea]
        , yVel <- [minYVel targetArea .. maxYVel targetArea]
        ]
