-- Tasty makes it easy to test and allow combine many different types of tests into one suite. <http://documentup.com/feuerbach/tasty>
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for writing tests. <https://hspec.github.io>
import Test.Tasty.Hspec

import LambdaCalculus.Lambda
import LambdaCalculus.Parse (parseLambdaTerm)

main :: IO ()
main = do
    test <- testSpec "lambda-calculus-tests" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do

  describe "Free Variables" $ do
    it "free variables of x should be x" $ do
        allFreeVariables (parseLambdaTerm "x") `shouldBe` [(Var "x")]

    it "free variables of λx.y should be y" $ do
        allFreeVariables (parseLambdaTerm "\\x.y") `shouldBe` [(Var "y")]

    it "allFreeVariables of ((λx.y) a) should be [y, a]" $ do
        allFreeVariables (parseLambdaTerm "(\\x.y) a") `shouldBe` [(Var "y"), (Var "a")]