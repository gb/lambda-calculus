module LambdaCalculus.Lambda
(
  allFreeVariables,
  freshVariable
) where

import Data.List (union, (\\))
import LambdaCalculus.Core (LambdaTerm(..), Var(..))

-- The set of all free variables of a lambda term
allFreeVariables :: LambdaTerm -> [Var]
allFreeVariables (Term v) = [v]
allFreeVariables (v :-> t) = allFreeVariables t \\ [v]
allFreeVariables (t1 :$ t2) = allFreeVariables t1 `union` allFreeVariables t2

-- Find a value not present in the current set of free variables to ensure substitution doesn't change the meaning of the function
freshVariable :: LambdaTerm -> Var
freshVariable t = head (variables \\ terms t)
  where variables = map (\x -> Var [x]) (['a'..'z'] ++ ['A'..'Z'])
        terms (Term v) = [v]
        terms (t1 :$ t2) = terms t1 `union` terms t2
        terms (v :-> l)  = terms l `union` [v]