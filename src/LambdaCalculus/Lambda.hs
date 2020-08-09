module LambdaCalculus.Lambda
( Var(..),
  LambdaTerm(..),
  allFreeVariables
) where

import Data.List (union, (\\))

data Var = Var String deriving Eq
instance Show Var where show (Var x) = x

data LambdaTerm = Var :-> LambdaTerm -- Abstraction
            | LambdaTerm :$ LambdaTerm -- Application
            | Term Var -- Variable

instance Show LambdaTerm where
  show (v :-> t) = "(λ" ++ show v ++ "." ++ show t ++ ")"
  show (t1 :$ t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Term v) = show v

-- The set of all free variables of a lambda term
allFreeVariables :: LambdaTerm -> [Var]
allFreeVariables (Term v) = [v]
allFreeVariables (v :-> t) = allFreeVariables t \\ [v]
allFreeVariables (t1 :$ t2) = allFreeVariables t1 `union` allFreeVariables t2