module LambdaCalculus.Core
(
  Var(..),
  LambdaTerm(..),
) where

data Var = Var String deriving Eq
data LambdaTerm = Var :-> LambdaTerm -- Abstraction
            | LambdaTerm :$ LambdaTerm -- Application
            | Term Var -- Variable

instance Show Var where show (Var x) = x
instance Show LambdaTerm where
  show (v :-> t) = "λ" ++ show v ++ "." ++ show t
  show (t1 :$ t2) = show t1 ++ " " ++ show t2
  show (Term v) = show v