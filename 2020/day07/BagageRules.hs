module BagageRules where

import           Data.Map  (Map, (!))
import qualified Data.Map  as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

type BagColor = Text

data RequiredContents = RequiredContents
  { amount :: Int
  , color  :: BagColor
  } deriving (Show)

parseRequiredContents :: Text -> RequiredContents
parseRequiredContents text =
  let parts = T.words text
   in RequiredContents
        { amount = parseInt $ head parts
        , color = T.unwords $ take 2 $ tail parts
        }

type BagageRules = Map BagColor [RequiredContents]

parseBagageRules :: Text -> BagageRules
parseBagageRules = M.fromList . map parseBagageRule . T.lines
  where
    parseBagageRule :: Text -> (BagColor, [RequiredContents])
    parseBagageRule textLine =
      let parts = T.splitOn " bags contain " textLine
       in (head parts, parseAllRequirements $ parts !! 1)
    parseAllRequirements :: Text -> [RequiredContents]
    parseAllRequirements "no other bags." = []
    parseAllRequirements contents =
      map parseRequiredContents $ T.splitOn ", " contents

containsShinyGoldBag :: BagageRules -> BagColor -> Bool
containsShinyGoldBag rules bagColor =
  bagColor /= "shiny gold" && impliesShinyGoldBag bagColor
  where
    impliesShinyGoldBag "shiny gold" = True
    impliesShinyGoldBag outerBag =
      any (impliesShinyGoldBag . color) (rules ! outerBag)

totalContents :: BagageRules -> BagColor -> Int
totalContents rules bagColor = totalBags bagColor - 1
  where
    totalBags outerBag =
      1
        + sum
            (map (\RequiredContents {..} -> amount * totalBags color)
               $ rules ! outerBag)
