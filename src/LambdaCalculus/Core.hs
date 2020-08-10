module LambdaCalculus.Core
(
  Ide,
  LambdaTerm(..),
) where

type Ide = String
data LambdaTerm = Ide :-> LambdaTerm -- Abstraction
            | LambdaTerm :$ LambdaTerm -- Application
            | Term Ide -- Variable

instance Show LambdaTerm where
  show (v :-> t) = "(λ" ++ v ++ "." ++ show t ++ ")"
  show (t1 :$ t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Term v) = v