import qualified Test.Tasty
import Test.Tasty.Hspec

import LambdaCalculus.Core
import LambdaCalculus.Eval
import LambdaCalculus.Parse

main :: IO ()
main = do
    test <- testSpec "lambda-calculus-tests" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do

  describe "Free Variables" $ do
    it "free variables of x should be x" $ do
        allFreeVariables (parseLambdaTerm "x") `shouldBe` ["x"]

    it "free variables of λx.y should be y" $ do
        allFreeVariables (parseLambdaTerm "\\x.y") `shouldBe` ["y"]

    it "allFreeVariables of ((λx.y) a) should be [y, a]" $ do
        allFreeVariables (parseLambdaTerm "(\\x.y) a") `shouldBe` ["y", "a"]

  describe "Fresh Variable" $ do
    it "fresh variable of x should be a" $ do
        freshVariable (parseLambdaTerm "x") `shouldBe` "a"

    it "fresh variable of ((λx.y) a) should be b" $ do
        freshVariable (parseLambdaTerm "(\\x.y) a") `shouldBe` "b"

    it "fresh variable of ((λx.y) a b) should be c" $ do
        freshVariable (parseLambdaTerm "(\\x.y) a b") `shouldBe` "c"

  describe "Beta Reduction" $ do
    it "Function identity (λx.x) a should be reduced to: a" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.x) a")) `shouldBe` "a"

    it "(λx.x x) a should be reduced to: (a a)" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.x x) a")) `shouldBe` "(a a)"

    it "(λx.y x) a should be reduced to: (y a)" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.y x) a")) `shouldBe` "(y a)"

    it "(λx.λa.x) a should be reduced to: (λb.a)" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.\\a.x) a")) `shouldBe` "(λb.a)"

    it "(λx.λx.x) a should be reduced to: (λx.x)" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.\\x.x) a")) `shouldBe` "(λx.x)"

    it "(λx.(λy.y) x) a should be reduced to: ((λy.y) a)" $ do
        toString (betaReduction (parseLambdaTerm "(\\x.(\\y.y)x) a")) `shouldBe` "((λy.y) a)"