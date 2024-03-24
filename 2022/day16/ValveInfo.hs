module ValveInfo where

import           Data.Map  (Map)
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data ValveInfo = ValveInfo
  { flowRate  :: Int
  , tunnelsTo :: [Text]
  } deriving (Show)

type ValveInfos = Map Text ValveInfo

parseValveInfo :: Text -> (Text, ValveInfo)
parseValveInfo text =
  let parts = T.splitOn "; " text
      valveInfo = head parts
      tunnelsInfo = parts !! 1
      label = T.words valveInfo !! 1
      rateDescription = last $ T.words valveInfo
      tunnelList = last $ T.words $ T.replace ", " "," tunnelsInfo
   in ( label
      , ValveInfo
          { flowRate = parseUnsignedInt $ T.splitOn "=" rateDescription !! 1
          , tunnelsTo = T.splitOn "," tunnelList
          })

parseValves :: Text -> ValveInfos
parseValves = M.fromList . map parseValveInfo . T.lines

getJunctions :: ValveInfos -> [Text]
getJunctions = M.keys . M.filterWithKey (\k v -> k == "AA" || v.flowRate > 0)
