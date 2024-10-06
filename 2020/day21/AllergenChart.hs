module AllergenChart where

import           Control.Monad (guard)
import           Data.List     (foldl', foldl1')
import           Data.Map      (Map, fromSet)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Food          (Allergen, Food (..), Ingredient)

type PossibleAllergens = Map Allergen (Set Ingredient)

type AllergenChart = Map Allergen Ingredient

constructAllergenChart :: [Food] -> AllergenChart
constructAllergenChart = solveAllergens . findPossibleAllergens

findPossibleAllergens :: [Food] -> PossibleAllergens
findPossibleAllergens foods = fromSet possibleIngredients allAllergens
  where
    allAllergens = foldl' S.union S.empty $ map allergens foods
    possibleIngredients allergen =
      foldl1' S.intersection
        $ map ingredients
        $ filter (S.member allergen . allergens) foods

solveAllergens :: PossibleAllergens -> AllergenChart
solveAllergens = go
  where
    go possibilities =
      if M.null possibilities
        then M.empty
        else case M.lookupMin $ M.mapMaybe fromSingleTon possibilities of
               Nothing -> error "cannot resolve allergens"
               Just (allergen, ingredient) ->
                 let remainingPossibilities =
                       M.map (S.delete ingredient)
                         $ M.delete allergen possibilities
                  in M.insert allergen ingredient $ go remainingPossibilities
    fromSingleTon possibilities =
      guard (S.size possibilities == 1) >> Just (head $ S.toList possibilities)

safeIngredients :: AllergenChart -> Food -> Int
safeIngredients allergenChart Food {..} =
  S.size $ S.filter (`S.notMember` doNotEat) ingredients
  where
    doNotEat = S.fromList $ M.elems allergenChart
