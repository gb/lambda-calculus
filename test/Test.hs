-- Tasty makes it easy to test and allow combine many different types of tests into one suite. <http://documentup.com/feuerbach/tasty>
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for writing tests. <https://hspec.github.io>
import Test.Tasty.Hspec

import LambdaCalculus.Core
import LambdaCalculus.Lambda(allFreeVariables, freshVariable)
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

  describe "Fresh Variable" $ do
    it "fresh variable of x should be a" $ do
        freshVariable (parseLambdaTerm "x") `shouldBe` Var "a"

    it "fresh variable of ((λx.y) a) should be b" $ do
        freshVariable (parseLambdaTerm "(\\x.y) a") `shouldBe` Var "b"

    it "fresh variable of ((λx.y) a b) should be c" $ do
        freshVariable (parseLambdaTerm "(\\x.y) a b") `shouldBe` Var "c"