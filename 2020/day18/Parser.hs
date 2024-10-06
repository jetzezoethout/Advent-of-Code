module Parser where

import           Control.Applicative (Alternative (..), (<|>))
import           Control.Monad       (guard)
import           Control.Monad.State (StateT (StateT, runStateT))
import           Data.List           (foldl')
import           Data.Maybe          (fromJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read      (decimal)
import           Expression          (Expression (..), Operator (..))

type Parser = StateT Text Maybe

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)      = Nothing
rightToMaybe (Right value) = Just value

readChar :: Parser Char
readChar = StateT T.uncons

dropChar :: Char -> Parser ()
dropChar ch = do
  actual <- readChar
  guard $ actual == ch

readInt :: Parser Int
readInt = StateT $ rightToMaybe . decimal

readPlus :: Parser Operator
readPlus = dropChar '+' >> return Plus

readTimes :: Parser Operator
readTimes = dropChar '*' >> return Times

readOperator :: Parser Operator
readOperator = readPlus <|> readTimes

readAtomic :: Parser Expression
readAtomic = Atomic <$> readInt

sub :: Parser Expression -> Parser Expression
sub parser =
  readAtomic <|> do
    dropChar '('
    expression <- parser
    dropChar ')'
    return expression

buildLeftAssociative ::
     Parser Expression -> Parser Operator -> Parser Expression
buildLeftAssociative expressionParser operatorParser = do
  left <- expressionParser
  continuations <- many continue
  return $ foldl' (flip ($)) left continuations
  where
    continue = do
      dropChar ' '
      operator <- operatorParser
      dropChar ' '
      right <- expressionParser
      return $ \left -> Complex {..}

readExpression :: Parser Expression
readExpression = buildLeftAssociative (sub readExpression) readOperator

parseExpression :: Text -> Expression
parseExpression = fst . fromJust . runStateT readExpression

readAdvancedExpression :: Parser Expression
readAdvancedExpression = buildLeftAssociative readSum readTimes
  where
    readSum = buildLeftAssociative (sub readAdvancedExpression) readPlus

parseAdvancedExpression :: Text -> Expression
parseAdvancedExpression = fst . fromJust . runStateT readAdvancedExpression
