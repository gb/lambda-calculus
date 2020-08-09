module LambdaCalculus.Lambda
(
  allFreeVariables
) where

import Data.List (union, (\\))
import LambdaCalculus.Core (LambdaTerm(..), Var(..))

-- The set of all free variables of a lambda term
allFreeVariables :: LambdaTerm -> [Var]
allFreeVariables (Term v) = [v]
allFreeVariables (v :-> t) = allFreeVariables t \\ [v]
allFreeVariables (t1 :$ t2) = allFreeVariables t1 `union` allFreeVariables t2