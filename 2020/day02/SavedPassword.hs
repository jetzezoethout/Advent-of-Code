module SavedPassword where

import           Data.Text      (Text)
import qualified Data.Text      as T
import           PasswordPolicy (PasswordPolicy, checkWithBounds,
                                 checkWithPositions, parsePasswordPolicy)

data SavedPassword = SavedPassword
  { policy   :: PasswordPolicy
  , password :: Text
  }

parseSavedPassword :: Text -> SavedPassword
parseSavedPassword text =
  let parts = T.splitOn ": " text
   in SavedPassword
        {policy = parsePasswordPolicy $ head parts, password = parts !! 1}

isValid :: SavedPassword -> Bool
isValid SavedPassword {..} = checkWithBounds policy password

isActuallyValid :: SavedPassword -> Bool
isActuallyValid SavedPassword {..} = checkWithPositions policy password
