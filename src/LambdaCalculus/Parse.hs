module LambdaCalculus.Parse (parseLambda, parseLambdaTerm) where

import LambdaCalculus.Lambda (LambdaTerm(..), Var(..))
import Text.Parsec
import Data.Functor.Identity
import Control.Monad (liftM)

abstraction :: ParsecT String () Identity LambdaTerm
abstraction = do
  _ <- oneOf "λ\\"
  skipMany space
  vars <- variable `sepBy` commaSeparated
  skipMany space
  _ <- char '.'
  skipMany space
  body <- application
  return (foldr (:->) body vars)

application :: ParsecT String () Identity LambdaTerm
application = do
  skipMany space
  expression <- lambdaExpression `endBy` (many space)
  skipMany space
  return (foldl1 (:$) expression)

commaSeparated :: ParsecT String () Identity ()
commaSeparated = do
  skipMany space
  _ <- char ','
  skipMany space

lambdaExpression :: ParsecT String () Identity LambdaTerm
lambdaExpression = try term
    <|> (try $ between (char '(') (char ')') application)
    <|> abstraction
    <?> "lambda expression"

parseLambda :: String -> Either ParseError LambdaTerm
parseLambda = parse application "(lambda)"

parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm x = case parseLambda x of
                      Left _ -> Term (Var "error")
                      Right t -> t

term :: ParsecT String () Identity LambdaTerm
term = liftM Term variable

variable :: ParsecT String () Identity Var
variable = do
  t <- many1 (noneOf "()λ\\,. \n\t")
  return (Var t)