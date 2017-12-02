module Day2.Day2Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Day2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day2e1" $ do
    it "calculates day2e1 properly" $ do
      day2e1 input `shouldBe` 41919
  describe "day2e2" $ do
    it "calculates day2e2 properly" $ do
      day2e2 input `shouldBe` 303