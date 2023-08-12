module MyLibSpec (spec) where

import MyLib
import RIO
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "should return unit" $ do
      sayHello `shouldReturn` ()
  describe "(+) @Int" $ do
    it "is commutative" $ property $ do
      \(x :: Int) (y :: Int) -> do
        x + y `shouldBe` y + x
