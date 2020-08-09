module LambdaCalculus.Lambda
(
  allFreeVariables,
  betaReduction,
  freshVariable
) where

import Control.Monad (mplus)
import Data.List (union, (\\))
import LambdaCalculus.Core (LambdaTerm(..), Var(..))

-- The set of all free variables of a lambda term
allFreeVariables :: LambdaTerm -> [Var]
allFreeVariables (Term v) = [v]
allFreeVariables (v :-> t) = allFreeVariables t \\ [v]
allFreeVariables (t1 :$ t2) = allFreeVariables t1 `union` allFreeVariables t2

-- Replace a bound variable in a function body with a function argument
betaReduction :: LambdaTerm -> Maybe LambdaTerm
betaReduction (Term _) = Nothing
betaReduction (x :-> t) = betaReduction t >>= (\r -> return (x :-> r))
betaReduction ((x :-> t1) :$ t2) = Just $ substitution x t2 t1
betaReduction (t1 :$ t2) = (betaReduction t1 >>= (\r1 -> return (r1 :$ t2)))
                `mplus` (betaReduction t2 >>= (\r2 -> return (t1 :$ r2)))

-- Replace all free occurrences of a variable in an term with another term
substitution :: Var -> LambdaTerm -> LambdaTerm -> LambdaTerm
substitution x n m@(Term y)
  | x == y = n
  | otherwise = m
substitution x n (t1 :$ t2) = substitution x n t1 :$ substitution x n t2
substitution x n @m(y :-> t)
  | x == y = m
  | x `notElem` allFreeVariables t = m
  | y `notElem` allFreeVariables n = y :-> substitution x n t
  | otherwise = z :-> substitution x n (substitution y (Term z) t)
    where z = freshVariable (n :$ t)

-- Find a value not present in the current set of free variables to ensure substitution doesn't change the meaning of the function
freshVariable :: LambdaTerm -> Var
freshVariable t = head (variables \\ terms t)
  where variables = map (\x -> Var [x]) (['a'..'z'] ++ ['A'..'Z'])
        terms (Term v) = [v]
        terms (t1 :$ t2) = terms t1 `union` terms t2
        terms (v :-> l)  = terms l `union` [v]