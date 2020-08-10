module LambdaCalculus.Parse
(
  parseLambda,
  parseLambdaTerm,
  toString
) where

import LambdaCalculus.Core (Ide, LambdaTerm(..))
import Text.Parsec
import Data.Functor.Identity
import Control.Monad

abstractionParse :: ParsecT String () Identity LambdaTerm
abstractionParse = do
  _ <- oneOf "λ\\"
  skipMany space
  vars <- variableParse `sepBy` commaSeparatedParse
  skipMany space
  _ <- char '.'
  skipMany space
  body <- applicationParse
  return (foldr (:->) body vars)

applicationParse :: ParsecT String () Identity LambdaTerm
applicationParse = do
  skipMany space
  expression <- lambdaExpressionParse `endBy` (many space)
  skipMany space
  return (foldl1 (:$) expression)

commaSeparatedParse :: ParsecT String () Identity ()
commaSeparatedParse = do
  skipMany space
  _ <- char ','
  skipMany space

lambdaExpressionParse :: ParsecT String () Identity LambdaTerm
lambdaExpressionParse = try termParse
    <|> (try $ between (char '(') (char ')') applicationParse)
    <|> abstractionParse
    <?> "lambda expression"

termParse :: ParsecT String () Identity LambdaTerm
termParse = liftM Term variableParse

variableParse :: ParsecT String () Identity Ide
variableParse = do
  t <- many1 (noneOf "()λ\\,. \n\t")
  return t

parseLambda :: String -> Either ParseError LambdaTerm
parseLambda = parse applicationParse "(lambda)"

parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm x = case parseLambda x of
                      Left _ -> Term ("error")
                      Right t -> t

toString :: Maybe LambdaTerm -> String
toString x = case x of
                  Just t -> show t
                  Nothing -> "error"