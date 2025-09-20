module Utils.ListSpec (spec) where

import Test.Hspec

import Utils.List ((!?))

spec :: Spec
spec = describe "Spec" $ do
    describe "!?" $ do
        it "indexes" $ do
            [0, 1, 2] !? (1 :: Int) `shouldBe` Just 1
        it "fails for out-of-bound indexes" $ do
            [0, 1, 2] !? (10 :: Int) `shouldBe` Nothing
        it "works for `Maybe` index" $ do
            [0, 1, 2] !? (Just 1 :: Maybe Int) `shouldBe` Just 1
        it "fails for `Nothing` index" $ do
            [0, 1, 2] !? (Nothing :: Maybe Int) `shouldBe` Nothing
