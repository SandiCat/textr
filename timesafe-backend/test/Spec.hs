module Spec where

import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "post" $ do
        it "works" $ True `shouldBe` True
