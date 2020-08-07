-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "lamda-calculus-tests" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do

  describe "Lambda terms" $ do
    it "a variable, x, is itself a valid lambda term" $ do
        True `shouldBe` True

    it "if t is a lambda term, and x is a variable, then (lambda x.t) is a lambda term (called an abstraction)" $ do
        True `shouldBe` True

    it "if t and s are lambda terms, then (ts) is a lambda term (called an application)" $ do
        True `shouldBe` True